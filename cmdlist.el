;;; cmdlist.el --- Automated latex command maintenance

;; Copyright © 2020-2022 Joseph Helfer

;; Author: Joseph Helfer
;; URL: https://github.com/jojhelfer/cmdlist.el
;; Version: 1.2.2
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea is to maintain a global file with all the latex commands
;; you like to use. This package then provides tools to maintain that
;; file and automatically update your latex files based on it.

;; There is now also functionality which automatically adds
;; "usepackage" statements based on a global file listing which
;; packages provide which commands.

;; There are several convenience functions, most of which are listed
;; in the example keybindings below. The most import one is
;; cmdlist-conditional-update-latex-buffer. See the documentation of
;; those functions for more info, as well as the customization
;; variables below under "Variables".

;; Also see README.md (presently available at
;; https://github.com/jojhelfer/cmdlist.el) for more information.

;; My keybindings:

;; (evil-define-key 'normal LaTeX-mode-map
;;   (kbd "SPC d c") 'cmdlist-update-save-and-compile
;;   (kbd "SPC g u") 'cmdlist-conditional-update-buffer
;;   (kbd "SPC g c") 'cmdlist-generate-and-add-cmd
;;   (kbd "SPC g a") 'cmdlist-add-cmd-to-file
;;   (kbd "SPC g r") 'cmdlist-generate-mathrm
;;   (kbd "SPC g b") 'cmdlist-generate-mathbf
;;   (kbd "SPC g B") 'cmdlist-generate-mathbb
;;   (kbd "SPC g C") 'cmdlist-generate-mathcal
;;   (kbd "SPC g F") 'cmdlist-generate-mathfrak
;;   (kbd "SPC g o") 'cmdlist-generate-operatorname
;;   (kbd "SPC g p") 'cmdlist-generate-package
;;   (kbd "SPC g z") 'cmdlist-delete-unused-newcmds
;;   (kbd "SPC g f") 'cmdlist-open-cmdlist-file)

;; TODO Look into just parsing the whole file, hopefully with some pre-existing tool. In particular, look into AucTeX's TeX-auto-file, TeX-auto-save, etc. We should also use TeX-find-macro-start and TeX-find-macro-end (and TeX-current-macro, etc.)
;; TODO Add commands to clean unused packages
;; TODO Try to guess command provider by looking through package/class files
;; TODO Ignore commands used in comments
;; TODO Be careful about commands (and packages) which are the empty string
;; TODO Check whether ".latex-commands.sty" and so on exist before using them
;; TODO Add \usetikzlibrary support
;; TODO Add option to (via elisp) allow presence of certain commands to trigger insertion of arbitrary code in preamble

(require 'cl-lib)
(require 'seq)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar cmdlist-base-directory (file-name-concat user-emacs-directory "cmdlist")
  "Default directory in which files with command definitions should be stored")

(defvar cmdlist-files `(,(file-name-concat cmdlist-base-directory "latex-commands.sty"))
  "List of files containing `\\\(re\)newcommand' entries to be used by `cmdlist.el'. The first entry is also used to store new commands.")

(defvar cmdlist-heading "% Commands"
  "Heading under which commands are inserted into the current buffer.")

(defvar cmdlist-visit-after-adding nil
  "If non-nil, visit cmdlist file after adding new command, unless value is `ask', in which case ask first.")

(defvar cmdlist-also-add-to-buffer nil
  "If non-nil, also add command to buffer when adding to global command list, unless value is `ask', in which case ask first.")

(defvar cmdlist-add-to-file-default nil
  "If non-nil, add new commands to cmdlist file by default (i.e., without prefix argument) rather than to current buffer.")

(defvar cmdlist-braces-around-cmd-name nil
  "If non-nil, put braces around the command name (as in `\newcommand{\foo}' when adding new commands.")

(defvar cmdlist-theorem-file (file-name-concat cmdlist-base-directory "latex-theorems.sty")
  "File containing `\\newtheorem' entries to be used by `cmdlist'.")

(defvar cmdlist-theorem-heading "% Theorems"
  "Heading which `cmdlist-conditional-update-buffer' looks for in deciding whether to update theorems.")

(defvar cmdlist-default-shared-counter "defn"
  "Default shared counter for `newtheorem's.")

(defvar cmdlist-default-parent-counter nil
  "Default parent counter for `newtheorem's (if `cmdlist-default-shared-counter' is `nil').")

(defvar cmdlist-package-file (file-name-concat cmdlist-base-directory "latex-packages.sty")
  "File each of whose lines contains a `\\usepackage' or `\\documentclass' command followed by a comment with a comma-separated list of commands and environment it provides.")

(defvar cmdlist-package-heading "% Packages"
  "Heading under which packages are inserted into the current buffer.")

(defvar cmdlist-builtin-file (file-name-concat cmdlist-base-directory "latex-builtins")
  "File containing a list of built-in latex commands and environments, one per line.")

(defvar cmdlist-cmd-defining-cmds
  (list "let" "def" "newtheorem*" "foreach"
        (list "newif"
              (lambda (cmd)
                ;; Remove starting backslash if its there
                (when (string-prefix-p "\\" cmd) (setq cmd (substring cmd 1)))
                (append (list cmd)
                        (when (>= (length cmd) 2)
                          (let ((cmdnoif (substring cmd 2)))
                            (list cmd (concat cmdnoif "true") (concat cmdnoif "false"))))))))
  "List of commands (besides `\(re\)newcommand' and `newtheorem') which define a new command (with a backslash). Each entry can also be a list (cmd fun) where cmd is the name of the command, and fun takes its argument and returns a list of commands it defines.")

(defvar cmdlist-ignore-at-symbol t
  "If non-nil, `cmdlist-package-update-latex-buffer' will ignore commands containing `@'. (Note that, in any case, `@' is always treated as part of a command name, which will be incorrect if not within `\\makeatletter' region.)")

(defvar cmdlist-newcommands-to-ignore ()
  "List of commands that should not be taken from `cmdlist-files' by `cmdlist-update-latex-buffer'. This can be used, for example, to prevent a \"renewcommand\" of a builtin command from getting inserted.")

(defvar cmdlist-test-minimal-file "/tmp/minimal.tex"
  "Path of file created by `cmdlist-test-command-in-minimal-file' (which is called during `cmdlist-package-update-latex-buffer' when you come across an unrecognized command and select \"test it in a minimal LaTeX file\").")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cmdlist--dofilter (varbind &rest body)
  "Binds VAR to each element of LIST and performs BODY, returning the list of those elements for which result of BODY is non-nil.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  (let ((res (make-symbol "")))
    `(let ((,res))
       (dolist ,varbind
         (if (progn ,@body)
             (push ,(nth 0 varbind) ,res)))
       (reverse ,res))))

(defun cmdlist--zip1 (&rest args)
  "If ARGS are all non-empty lists, return (HEADS . TAILS), where HEADS is a list of the first elements, and TAILS is a list of the ends, otherwise return nil."
  (let ((heads) (tails)
        (good t))
    (dolist (x args)
      (if (not (and good x))
          (setq good nil)
        (push (car x) heads)
        (push (cdr x) tails)))
    (when good
      (cons (reverse heads) (reverse tails)))))

(defun cmdlist--zip (&rest args)
  (let ((res)
        (cur (apply 'cmdlist--zip1 args)))
    (while cur
      (push (car cur) res)
      (setq cur (apply 'cmdlist--zip1 (cdr cur))))
    (reverse res)))

(defun cmdlist--list-of-things (things &optional fmt)
  "Concatenates the strings in THINGS, separated by newlines with a number at the beginning of each line.
FMT specifies how the number should be formatted (default \"[%d]\")."
  (unless fmt
      (setq fmt "[%d] "))
  (let* ((nums (mapcar (lambda (x) (format fmt x)) (number-sequence 1 (length things))))
         (lines (cmdlist--zip nums things)))
    (mapconcat (lambda (x) (apply 'concat x)) lines "\n")))

(defun cmdlist--prompt-for-number (prompt &optional min max)
  "Keep displaying PROMPT and asking the user for input until the input is an integer (between MIN and MAX, if provided), then return it as an integer."
  (let ((response "")
        (res 0))
    (while (or (and min (< res min))
               (and max (> res max))
               (not (equal response (format "%d" res))))
      (setq response (read-from-minibuffer prompt))
      (setq res (string-to-number response)))
  res))

(defun cmdlist--forward-brexp ()
  "Move forward across one matching curly bracket expression."
  (interactive)
  (let ((started nil))
    (while (and (search-forward "{" nil nil)
                (if (eq (char-before (1- (point))) ?\\) t (setq started t) nil)))
    (when started
      (let ((count 1))
        (while (> count 0)
          (re-search-forward "[{}]")
          (unless (eq (char-before (1- (point))) ?\\)
            (setq count (+ count
                           (if (eq (char-before) ?\{) 1 -1)))))))))

(defmacro cmdlist--save-everything (&rest body)
  "Save mark-and-excursion, restriction, and match-data."
  (declare (indent 0) (debug t))
  `(save-mark-and-excursion
     (save-restriction
       (save-match-data
         ,@body))))

(defun cmdlist--read-lines (file)
  "Read lines of file into a list and return it, or nil if file does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t))))

;;;;;;;;;;;;;;;;;;;;;;
;; Parsing commands ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist--shloop-latex-arg ()
  "Move past the latex argument (bracket expression, command name consisting of characters `[A-Za-z@]', or (possibly escaped) single character) starting after point, and return it."
  ;; Move past any spaces and comments
  (while
      (cond ((eq (char-after) ? ) (forward-char) t))
    (cond ((eq (char-after) ?%) (forward-line) t)))
  (let ((start (point)))
    (pcase (char-after)
     (?\{ (cmdlist--forward-brexp))
     (?\\
      (forward-char)
      (if (string-match-p "[A-Za-z@]" (string (char-after)))
          (re-search-forward "[A-Za-z@]+")
        (forward-char)))
     (_ (forward-char)))
    (buffer-substring-no-properties start (point))))

(defun cmdlist--shloop-optional-latex-arg ()
  "Move past the optional latex argument starting after point, and return it (not including the square brackets). This assumes the char after point is `['. The end of the optional argument is by definition the first unescaped closing square bracket which is not inside of an (unescaped) curly brace expression."
  (forward-char)
  (let ((start (point)))
    (while (not (eq (char-after) ?\]))
      (pcase (char-after)
       (?\{ (cmdlist--forward-brexp))
       (?\\
        (forward-char 2))
       (_ (forward-char))))
    (forward-char)
    (buffer-substring-no-properties start (1- (point)))))

(defun cmdlist--latex-cmd-under-point ()
  "If point is on a latex command, return its name, else nil"
  (let ((start (point)))
    (cmdlist--save-everything
      (when (< (point) (point-max))
        (forward-char))
      (when (search-backward "\\" nil t)
        (let ((cmd (cmdlist--shloop-latex-arg)))
          (when (<= start (point))
            (substring cmd 1)))))))

(defun cmdlist--search-backward-incl (string &optional as-regex)
  "Goto beginning of first instance of STRING occurring before or around point. Return nil if STRING was not found. If AS-REGEX is non-nill, treat STRING as a regex."
  (let ((backward-fun (if as-regex #'re-search-backward #'search-backward))
        (forward-fun (if as-regex #'re-search-forward #'search-forward))
        (start (point))
        (place)
        (mdata))
    (save-mark-and-excursion
      (if (funcall backward-fun string nil t)
          (progn (setq mdata (match-data))
                 (setq place (match-beginning 0))
                 (forward-char))
        (goto-char (point-min)))
      (while (and (funcall forward-fun string nil t)
                  (<= (match-beginning 0) start))
        (setq mdata (match-data))
        (setq place (match-beginning 0))
        (goto-char place)
        (forward-char)))
    (when place
      (set-match-data mdata)
      (goto-char place))))

(defun cmdlist--re-search-backward-incl (regex)
  "Goto beginning of first instance of REGEX occurring before or around point. Return nil if STRING was not found."
  (cmdlist--search-backward-incl regex t))

(defun cmdlist--surrounding-newcmd (&optional regex)
  "If point is not inside of a latex `\\\(re\)newcommand' (or given REGEX, e.g., `\\newtheorem'), return nil. Otherwise, return the text of the whole command. If the command definition is immediately followed by whitespace and then a comment, include that as well. This function will fail on nested command definitions."
  (unless regex (setq regex "\\\\\\(new\\|renew\\|provide\\)command"))
  (let ((start (point))
        (beg))
    (cmdlist--save-everything
      (when (cmdlist--re-search-backward-incl regex)
        (setq beg (point))
        (search-forward-regexp regex)
        (cmdlist--shloop-latex-arg)
        (dotimes (_ 2)
          (when (eq (char-after) ?\[)
            (cmdlist--shloop-optional-latex-arg)))
        (cmdlist--shloop-latex-arg)
        (when (>= (point) start)
          (when (string-match-p "^ *%" (buffer-substring (point) (line-end-position)))
            (end-of-line))
          (buffer-substring-no-properties beg (point)))))))

(defun cmdlist--scan-for-latex-cmds (&optional ignore-newcmds)
  "Return all names of latex commands in current buffer."
  (let (cmds last-newcmd-pos)
    (cmdlist--save-everything
      (when ignore-newcmds
        (goto-char (point-max))
        (if (not (search-backward "\\newcommand" nil t))
            (setq ignore-newcmds nil)
          (forward-char 11)
          (when (eq (char-after) ?\{)
            (cmdlist--forward-brexp))
          (cmdlist--forward-brexp)
          (setq last-newcmd-pos (point))))
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
        ;; Make sure it's not a double backslash
        (unless (and
                 (or (eq (char-after) ?\\)
                     ;; We were using `looking-back' here, but it turns out to be extremely slow
                     ;; (its documentation says as much).
                     ;; We were also using looking-at above, which seems to not be slow,
                     ;; but is still unnecessarily regexpful
                     (eq (char-after (- (point) 2)) ?\\))
                 ;; Or rather, make sure it's not an even number of backslashes
                 (let ((beg (progn (while (eq (char-before) ?\\) (backward-char)) (point)))
                       (end (progn (while (eq (char-after) ?\\) (forward-char)) (point))))
                   (cl-evenp (- end beg))))
          (let ((cmd (cmdlist--latex-cmd-under-point)))
            (unless (or (not cmd)
                        (equal cmd "")
                        (and ignore-newcmds
                             (< (point) last-newcmd-pos)
                             (let ((snc (cmdlist--surrounding-newcmd)))
                               (and snc
                                    (equal cmd (cmdlist--newcmd-name snc))))))
              (push cmd cmds)))))
    (reverse (delete-dups cmds)))))

(defun cmdlist--newcmd-name (cmd &optional regex)
  "Return the name of the given `\\\(re\)newcommmand' (or given REGEX, e.g., `\\newtheorem')."
  (cmdlist--save-everything
    (with-temp-buffer
      (insert cmd)
      (goto-char (point-min))
      (re-search-forward (or regex "\\\\\\(new\\|renew\\|provide\\)command"))
      (car (split-string (cmdlist--shloop-latex-arg) nil nil "[{}]*\\\\?")))))

(defun cmdlist--scan-for-newcmds ()
  "Return a list of all \\newcommands in the current buffer"
  (let ((res))
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(new\\|renew\\|provide\\)command[^a-z]" nil t)
        (push (cmdlist--surrounding-newcmd) res)
        (cmdlist--forward-brexp)))
    (reverse res)))

(defun cmdlist--scan-file-for-newcmds (file)
  "Return a list of all \\newcommmands in the given file"
  (save-mark-and-excursion
    (with-temp-buffer
      (insert-file-contents file)
      (cmdlist--scan-for-newcmds))))

(defun cmdlist--assemble-newcmd (name defn &optional numargs opt)
  "Return the `\\newcommand' defining NAME with definition DEFN. If NUMARGS or OPT are provided, they are the number of arguments and the default optional argument, respectively."
  (concat
   "\\newcommand"
   (when cmdlist-braces-around-cmd-name "{")
   "\\"
   name
   (when cmdlist-braces-around-cmd-name "}")
   (when (and numargs (> numargs 0)) (format "[%d]" numargs))
   (when opt  (format "[%s]" opt))
   "{" defn "}"))

(defun cmdlist--num-args-in-defn (defn)
  "Return the greatest argument number in the latex command definition DEFN. Only works for definitions with at most 9 arguments."
  (let ((maxnum 0))
    (cmdlist--save-everything
      (with-temp-buffer
        (insert defn)
        (goto-char (point-min))
        (while (search-forward "#" nil t)
          (let ((newnum (string-to-number (char-to-string (char-after)))))
            (when (> newnum maxnum)
              (setq maxnum newnum))))))
    maxnum))

(defun cmdlist--sort-newcmds (reverse beg end)
  "Variant of `sort-lines' (using the amazingly flexible `sort-subr') which keeps each (possibly multi-line) `\\newcommand' together, along with any lines before it up to the previous `\\newcommand'. Also ignores case."
  (interactive "P\nr")
  (cmdlist--save-everything
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ;; To make `end-of-line' and etc. to ignore fields.
        ((inhibit-field-text-motion t)
         (sort-fold-case t))
      (sort-subr reverse 'forward-line
                 (lambda ()
                   (cmdlist--re-search-backward-incl "\\\\r?e?newcommand")
                   (re-search-forward "\\\\r?e?newcommand")
                   (when (eq (char-after) ?\{) (cmdlist--forward-brexp))
                   (cmdlist--forward-brexp)
                   (end-of-line))
                 (lambda () (re-search-forward "\\\\r?e?newcommand{?") nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding commands in a list of \newcommands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist--get-cmdlist-matches (cmdlist name)
  "Return all elements of CMDLIST which are `\\\(re\)newcommand's defining the command NAME."
  (cmdlist--dofilter (cmd cmdlist)
    (or
     (string-prefix-p (concat "\\newcommand{\\" name "}") cmd)
     (string-prefix-p (concat "\\newcommand\\" name "[") cmd)
     (string-prefix-p (concat "\\newcommand\\" name "{") cmd)
     (string-prefix-p (concat "\\renewcommand{\\" name "}") cmd)
     (string-prefix-p (concat "\\renewcommand\\" name "[") cmd)
     (string-prefix-p (concat "\\renewcommand\\" name "{") cmd))))

(defun cmdlist--select-cmds-from-cmdlist (cmdlist names exceptions)
  "For each element of NAMES which is not in EXCEPTION, get all elements of CMDLIST which are `\\newcommand's defining this element, and return them in a list. Each time there are multiple matches, the user is queried for a choice."
  (let ((choices))
    (dolist (name names)
      (unless (member name exceptions)
        (let ((cmds (cmdlist--get-cmdlist-matches cmdlist name))
              (cmd))
          (if (> (length cmds) 1)
              (let ((prompt ""))
                (setq prompt (cmdlist--list-of-things cmds))
                (setq prompt (concat prompt "\nMultiple entries for " name ". Choose from above: "))
                (setq cmd (nth
                           (- (cmdlist--prompt-for-number prompt 1 (length cmds)) 1)
                           cmds)))
            (when cmds
              (setq cmd (nth 0 cmds))))
          (when cmd
            (push cmd choices)))))
    choices))

(defun cmdlist--replace-prompt (cmdlist entry)
  "If the command defined in ENTRY is not in CMDLIST, return `t'. Otherwise, display all matches and ask the user to proceed. The user may select an entry to replace, which is then returned, cancel, in which case `nil' is return, or choose to add a new entry, in which `t' is returned."
  (let* ((name (cmdlist--newcmd-name entry))
         (matches (cmdlist--get-cmdlist-matches cmdlist name))
         (many (< 1(length matches))))
    (if (not matches) t
      (cl-case (car (read-multiple-choice
                  (concat "\"" name "\" already defined. Previous definition"
                          (when many "s")
                          ":\n"
                          (cmdlist--list-of-things matches "  ")
                          "\n\nWhat would you like to do?")
                  `((?q "quit")
                    (?r ,(concat "replace " (when many "an ") "existing definition"))
                    (?a "add a new definition"))))
        (?q nil)
        (?a t)
        (?r (cmdlist--singleton-or-prompt matches "Which definition to replace? "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying buffer and adding new commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist--stick-at-top (heading text &optional sortfun)
  "Stick TEXT (or each string in TEXT) after the first occurrence of HEADING, then sort it using SORTFUN if non-nil.
If HEADING does not occur, first insert HEADING before \\begin{document}"
  (let ((starting-point))
    (cmdlist--save-everything
      (if (buffer-narrowed-p) (widen))
      (goto-char (point-min))
      (unless (search-forward heading nil t)
        (search-forward "\\begin{document}")
        (beginning-of-line)
        (insert (concat heading "\n\n"))
        (forward-line -2))
      (forward-line)
      (setq starting-point (point))
      (forward-line -1)
      (search-forward "\n\n" nil t)
      (forward-line -1)
      (when (not (listp text))
        (setq text (list text)))
      (dolist (tx text)
        (insert (concat tx "\n")))
      (when sortfun
        (funcall sortfun nil starting-point (point))))))

(defun cmdlist--add-to-cmdlist-file (newcmd &optional file)
  "Add NEWCMD to FILE (default: the first entry in `cmdlist-files') and then sort the lines.

If the command being defined in NEWCMD is already in FILE, ask for confirmation."
  (unless file (setq file (car cmdlist-files)))
  (if (and (file-readable-p file) (file-writable-p file))
      (let ((cmdlist (cmdlist--scan-file-for-newcmds file)))
        (let ((action (cmdlist--replace-prompt cmdlist newcmd)))
          (if action
              (cmdlist--save-everything
                (let ((fibuf (find-file-noselect file)))
                  (with-current-buffer fibuf
                    (unless (eq t action)
                      (goto-char (point-min))
                      (search-forward (concat action "\n"))
                      (delete-region (match-beginning 0) (match-end 0)))
                    (goto-char (point-min))
                    (insert newcmd)
                    (insert "\n")
                    (cmdlist--sort-newcmds nil (point-min) (point-max))
                    (save-buffer))
                  (if (not cmdlist-visit-after-adding)
                      (kill-buffer fibuf)
                    (unless (and (eq cmdlist-visit-after-adding 'ask)
                                 (not (y-or-n-p
                                       (format "New entry\n  %s\nadded to file %s\nVisit? "
                                               newcmd file))))
                      (switch-to-buffer-other-window fibuf)
                      (search-forward newcmd)
                      (beginning-of-line))))
                (message "New entry\n  %s\nadded to file %s" newcmd file))
            (message "Operation cancelled."))))
    (message "File %s does not exist or not readable/writable." file)))

(defun cmdlist--add-newcmd (newcmd &optional heading file)
  "Add the given new command to the top of the file under HEADING (default: `cmdlist-heading'), asking for confirmation if already defined.

If FILE is given, instead add it to FILE. If FILE is `t', it is set to the first element of `cmdlist-files'."
  (if file
      (progn
        (unless (stringp file) (setq file (car cmdlist-files)))
        (cmdlist--add-to-cmdlist-file newcmd file))
    (unless heading (setq heading cmdlist-heading))
    (let ((action (cmdlist--replace-prompt (cmdlist--scan-for-newcmds) newcmd)))
      (if (not action)
          (message "Operation cancelled")
        (unless (eq t action)
          (cmdlist--save-everything
            (goto-char (point-min))
            (search-forward (concat action "\n"))
            (delete-region (match-beginning 0) (match-end 0))))
        (cmdlist--stick-at-top heading newcmd 'cmdlist--sort-newcmds)
        (message "New entry\n  %s\nadded under %s ." newcmd heading)))))

(defun cmdlist--generate-newcmd ()
  "Generate a `\\newcommand' and return it. The name and definition of the new macro are queried for.

The number of arguments is guessed by how many are used in the command. If there is at least one, query for an optional argument.

If there is a macro under the current point, the default name is
this macro."
  (let* ((name (cmdlist--latex-cmd-under-point))
         (defn)
         (numargs)
         (optional))
    (if (not name)
        (setq name ""))
    (setq name (read-from-minibuffer "Name of new command? \\" name))
    (setq defn (read-from-minibuffer (concat "Definition of command \\" name " ? ")))
    (setq numargs (cmdlist--num-args-in-defn defn))
    (when (and (> numargs 0)
               (y-or-n-p "Optional argument? "))
      (setq optional
            (read-from-minibuffer (format "Default optional argument for \\%s[%d]? "
                                          name numargs))))
    (cmdlist--assemble-newcmd name defn numargs optional)))

(defun cmdlist-update-latex-buffer (&optional heading files prefix)
  "Add to current buffer under HEADING all commands defined in FILES which are present in the current file and not already defined. If PREFIX is non-nil, it is prepended to `\\newcommand'.

By default HEADING is `cmdlist-heading' and FILES are the files in the variable `cmdlist-files'."
  (interactive)
  (unless heading (setq heading cmdlist-heading))
  (unless files (setq files cmdlist-files))
  (unless prefix (setq prefix ""))
  (let* ((cmdlist (apply 'append (mapcar 'cmdlist--scan-file-for-newcmds files)))
         (cmds (cmdlist--scan-for-latex-cmds))
         (exceptions (append (mapcar 'cmdlist--newcmd-name (cmdlist--scan-for-newcmds)) (cmdlist--scan-for-other-defined-cmds) cmdlist-newcommands-to-ignore))
         (newcmds (cmdlist--select-cmds-from-cmdlist cmdlist cmds exceptions)))
    (if newcmds
        (progn
          (setq newcmds (mapcar (lambda (x) (concat prefix x)) newcmds))
          (cmdlist--stick-at-top heading newcmds 'cmdlist--sort-newcmds)
          (message "Added %d new commands:\n%s" (length newcmds) (cmdlist--list-of-things newcmds)))
      (message "No commands added."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist-add-cmd-to-file ()
  "Add the `\\newcommand' definition under point to the first entry of `cmdlist-files'."
  (interactive)
  (let ((newcmd (cmdlist--surrounding-newcmd)))
    (when newcmd
      (cmdlist--add-newcmd (cmdlist--surrounding-newcmd) nil t))))

(defun cmdlist-generate-and-add-cmd (tofile)
    "Generate a `\\newcommand' with `cmdlist--generate-newcmd' and add it to current file under `cmdlist-heading'. With or without prefix argument (depending on `cmdlist-add-to-file-default'), add to first entry of `cmdlist-files' instead."
    (interactive "P")
    (when cmdlist-add-to-file-default
      (setq tofile (not tofile)))
    (let* ((newcmd (cmdlist--generate-newcmd))
           (tobuf (and tofile
                       cmdlist-also-add-to-buffer
                       (if (eq cmdlist-also-add-to-buffer 'ask)
                           (y-or-n-p (format "Also add\n  %s\nto buffer?" newcmd)) t))))
      (cmdlist--add-newcmd newcmd nil (when tofile t))
      (when tobuf
        (cmdlist--add-newcmd newcmd nil nil))))

(defun cmdlist-generate-and-add-fontletter (font &optional heading file)
  "Generate a ``\\newcommand'' of the form `\\newcomand\\name{\\FONT{letter}}' and add it to the current buffer or to FILE with `cmdlist--add-newcmd' (hence FILE can be `t').

The name and letter are queried for, and by default are both the latex macro under point."
  (let* ((name (read-from-minibuffer
                (format "Name of %s command? \\" font) (cmdlist--latex-cmd-under-point)))
         (letter (read-from-minibuffer "Text? " name)))
    (cmdlist--add-newcmd (cmdlist--assemble-newcmd name (format "\\%s{%s}" font letter)) heading file)))

(defun cmdlist-generate-mathbb (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `mathbb'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "mathbb" nil (when tofile t)))

(defun cmdlist-generate-mathbf (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `mathbf'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "mathbf" nil (when tofile t)))

(defun cmdlist-generate-mathrm (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `mathrm'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "mathrm" nil (when tofile t)))

(defun cmdlist-generate-mathcal (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `mathcal'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "mathcal" nil (when tofile t)))

(defun cmdlist-generate-mathfrak (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `mathfrak'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "mathfrak" nil (when tofile t)))

(defun cmdlist-generate-operatorname (tofile)
  "Run `cmdlist-generate-and-add-fontletter' with `operatorname'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (cmdlist-generate-and-add-fontletter "operatorname" nil (when tofile t)))

(defun cmdlist-generate-package ()
  "Ask for a package name and stick it under `% Packages'"
  (interactive)
  (let ((name (read-from-minibuffer "Package name? ")))
    (cmdlist--stick-at-top "% Packages" (concat "\\usepackage{" name "}"))))

(defun cmdlist-get-unused-newcmds (&optional whole-buffer)
  "Return a list of `\\newcommand's in this buffer which are (apparently) not being used. If whole-buffer is nil, restrict to the commands under `cmdlist-heading'."
  (let* ((cmds (cmdlist--scan-for-latex-cmds t))
         (newcmds
          (if whole-buffer
              (cmdlist--scan-for-newcmds)
            (cmdlist--save-everything
              (goto-char (point-min))
              (search-forward cmdlist-heading)
              (let ((st (point)))
                (search-forward "\n\n")
                (narrow-to-region st (point))
                (cmdlist--scan-for-newcmds)))))
         (unuseds (cmdlist--dofilter (x newcmds)
                    (not (member (cmdlist--newcmd-name x) cmds)))))
    unuseds))

(defun cmdlist-show-unused-newcmds ()
  "Display `\\newcommand's in this buffer which are (apparently) not being used."
  (interactive)
  (let ((unuseds (append (cmdlist-get-unused-newcmds) (cmdlist--get-unused-newthms))))
    (if unuseds
        (message "Seemingly unused commands:\n\n%s" (mapconcat #'identity unuseds "\n"))
      (message "No unused commands found"))))

(defun cmdlist-delete-unused-newcmds ()
  "Delete (seemingly) unused `\\\(re\)newcommand's and `\\\\newtheorem's in this buffer, after prompting."
  (interactive)
  (let ((unuseds (append (cmdlist-get-unused-newcmds) (cmdlist--get-unused-newthms))))
    (if unuseds
        (when (y-or-n-p (format "Seemingly unused commands:\n%s\nDelete? " (cmdlist--list-of-things unuseds)))
          (dolist (x unuseds)
            (cmdlist--save-everything
              (goto-char (point-min))
              (let ((case-fold-search nil))
                (search-forward x))
              ; instead of kill-line, to avoid changing kill-ring
              (delete-region (progn (beginning-of-line) (point))
                             (1+ (progn (end-of-line) (point)))))))
      (message "No unused commands found"))))

(defun cmdlist-open-cmdlist-file ()
  "`(find-file-other-window (car cmdlist-files))'"
  (interactive)
  (let ((cmd (cmdlist--latex-cmd-under-point)))
    (find-file-other-window (car cmdlist-files))
    ;; Use swiper if possible
    (if (require 'swiper nil t)
        (swiper)
      (setq cmd (completing-read "Goto command: "
                                 (mapcar 'cmdlist--newcmd-name (cmdlist--scan-for-newcmds))
                                 nil nil nil nil cmd))
      (when cmd
        (let ((cmd-pos))
          (cmdlist--save-everything
            (goto-char (point-min))
            (when (re-search-forward (format "\\\\r?e?newcommand{?\\\\%s[}{\\[]" cmd) nil t)
              (setq cmd-pos (point))))
          (when cmd-pos
            (goto-char cmd-pos)))))))

(defun cmdlist-conditional-update-buffer ()
  "If `cmdlist-heading' is present in the buffer, run `cmdlist-update-latex-buffer'. If furthermore either of `cmdlist-theorem-heading' or `cmdlist-package-heading' is present, run `cmdlist-newthm-update-latex-buffer' or `cmdlist-package-update-latex-buffer', respectively."
  (interactive)
  (let ((has-heading nil)
        (has-thm-heading nil)
        (has-pkg-heading nil))
    (save-mark-and-excursion
      (goto-char (point-min))
      (setq has-heading (re-search-forward (concat "^" cmdlist-heading "$") nil t))
      (goto-char (point-min))
      (setq has-thm-heading (re-search-forward (concat "^" cmdlist-theorem-heading "$") nil t))
      (goto-char (point-min))
      (setq has-pkg-heading (re-search-forward (concat "^" cmdlist-package-heading "$") nil t)))
    (when has-heading
      (cmdlist-update-latex-buffer)
      (when has-thm-heading
        (cmdlist-newthm-update-latex-buffer))
      (when has-pkg-heading
        (cmdlist-package-update-latex-buffer)))))

(defun cmdlist-update-save-and-compile ()
  "Save buffer, run `cmdlist-conditional-update-buffer', and then compile with `Tex-command'."
  (interactive)
  (cmdlist-conditional-update-buffer)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments and theorems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist--scan-for-latex-envs ()
  "Return all names of latex environments in current buffer."
  (let (envs)
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (search-forward "\\begin{" nil t)
        (backward-char)
        (push (car (split-string (cmdlist--shloop-latex-arg) nil nil "[{}]")) envs)))
    (reverse (delete-dups envs))))

(defun cmdlist--stick-thm-at-top (text &optional style shared-counter parent-counter)
  "Run cmdlist--stick-at-top with TEXT and with heading set to `\theoremstyle{STYLE}' (by default, STYLE is read from a comment at the end of text, which is removed, and is otherwise `plain'), and with no sorting. If SHARED-COUNTER is provided, add it as an optional argument after the first argument. Otherwise, if PARENT-COUNTER is provided, add it as an optional argument after the second argument. Otherwise, if `cmdlist-default-shared-counter' is non-nil, use that as SHARED-COUNTER. Otherwise, if `cmdlist-default-parent-counter' is non-nil, use that as PARENT-COUNTER."
  (unless (or shared-counter parent-counter)
    (setq shared-counter cmdlist-default-shared-counter)
    (setq parent-counter cmdlist-default-parent-counter))
  (with-temp-buffer
    (insert text)
    ;; Get style if present. Else default to `plain'.
    (goto-char (point-min))
    (if (re-search-forward "%\\(.*\\)" nil t)
        (progn (unless style (setq style (match-string-no-properties 1)))
               (replace-match ""))
      (unless style (setq style "plain")))
    ;; Handle counter
    (goto-char (point-min))
    (when (and (or shared-counter parent-counter)
               (re-search-forward "\\[[^\\]*\\]" nil t))
      (replace-match ""))
    (goto-char (point-min))
    ;; If shared-counter is the current theorem, don't add it.
    (unless (and shared-counter (search-forward (concat "{" shared-counter "}") nil t))
      (goto-char (point-min))
      (when (search-forward "}")
        (if shared-counter
            (insert "[" shared-counter "]")
          (when (and parent-counter (search-forward "}"))
            (insert "[" parent-counter "]")))))
    (setq text (buffer-substring-no-properties (point-min) (point-max))))
  (cmdlist--stick-at-top (concat "\\theoremstyle{" style "}") text))

(defun cmdlist--scan-for-newthms ()
  "Return a list of all `\\newtheorem's in the current buffer"
  ;; This is adapted from cmdlist--scan-for-newcmds. Perhaps something should be factored out?
  (let ((res))
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\newtheorem[^a-z]" nil t)
        (push (cmdlist--surrounding-newcmd "\\\\newtheorem\\*?") res)
        (cmdlist--forward-brexp)))
    (reverse res)))

(defun cmdlist--get-unused-newthms ()
  "Return a list of `\\newtheorem's in this buffer which are (apparently) not being used."
  ;; This is adapted from `cmdlist-get-unused-newcmds'. Perhaps something should be factored out?
  (let* ((envs (append (cmdlist--scan-for-latex-envs) (cmdlist--get-newtheorem-dependencies)))
         (newthms (cmdlist--scan-for-newthms))
         (unuseds (cmdlist--dofilter (x newthms)
                    (not (member (cmdlist--newcmd-name x "\\newtheorem\\*?") envs)))))
    unuseds))

(defun cmdlist--scan-file-for-newthms (file)
  "Return a list of all `\\newtheorem's in the given file"
  ;; This is adapted from `cmdlist--scan-file-for-newcmds'. Perhaps something should be factored out?
  (save-mark-and-excursion
    (with-temp-buffer
      (insert-file-contents file)
      (cmdlist--scan-for-newthms))))

(defun cmdlist--get-newtheorem-dependencies ()
  "Get all the commands included in square brackets in newtheorem commands."
    (let (res)
      (cmdlist--save-everything
        (goto-char (point-min))
        (while (re-search-forward "\\\\newtheorem\\*?{[a-zA-Z]*}\\[\\([a-zA-Z]*\\)\\]" nil t)
          (add-to-list 'res (match-string-no-properties 1)))
        res)))

(defun cmdlist-newthm-update-latex-buffer (&optional file)
  "Add to current buffer all ``\\newtheorem's defined in FILE (using `cmdlist--stick-thm-at-top') which are present in the current file and not already defined. By default FILE is `cmdlist-theorem-file'."
  ;; This is adapted from `cmdlist-update-latex-buffer'. Perhaps something should be factored out?
  (interactive)
  (unless file (setq file cmdlist-theorem-file))
  (let* ((thmlist (cmdlist--scan-file-for-newthms file))
         (envs (append (cmdlist--scan-for-latex-envs) (cmdlist--get-newtheorem-dependencies)))
         (exceptions (mapcar #'(lambda (x) (cmdlist--newcmd-name x "\\newtheorem\\*?")) (cmdlist--scan-for-newthms)))
         (newthms
          (cmdlist--dofilter (thm thmlist)
            (and (member (cmdlist--newcmd-name thm "\\newtheorem\\*?") envs)
                 (not (member (cmdlist--newcmd-name thm "\\newtheorem\\*?") exceptions))))))
    (if newthms
        (progn
          (dolist (thm newthms)
            (cmdlist--stick-thm-at-top thm))
          (message "Added %d new theorem:\n%s" (length newthms) (cmdlist--list-of-things newthms)))
      (message "No theorems added."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages and built-ins ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmdlist--package-and-class-sort ()
  "Sort lines in buffer ignoring initial `\\usepackage' or `\\documentclass'."
  (cmdlist--save-everything
    (goto-char (point-min))
    (let ((sort-fold-case t))
      (sort-subr nil 'forward-line 'end-of-line
                 (lambda ()
                   (save-match-data
                     (and (re-search-forward
                           "^\\\\\\(usepackage\\|documentclass\\)" nil t) nil)))))))

(defun cmdlist--scan-package-file (packages &optional recurse file pkgs-instead)
  "Return a list of all comma-separated entries appearing after `%'s in FILE (default is `cmdlist-package-file') in lines defining packages in PACKAGES. If RECURSE is non-nil, also recursively include entries appearing in packages appearing after the second `%' in some line. If PKGS-INSTEAD is non-nil, return the entries after the second `%' instead of the first."
  (unless file (setq file cmdlist-package-file))
  ;; If we're recursing, add all packages.
  (when recurse
    (let ((new-pkgs packages))
      (while new-pkgs
        (let ((newer-pkgs ()))
          (dolist (p (cmdlist--scan-package-file new-pkgs nil file t))
            (when (not (member p packages))
              (push p packages)
              (push p newer-pkgs)))
          (setq new-pkgs newer-pkgs)))))
  ;; Make sure we're sorted
  ;; Warning! (sort) is destructive! (It returns the sorted list, but mangles the original one!)
  (setq packages (seq-sort 'string< packages))
  (let ((result ()))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Make sure we're sorted here too
      (cmdlist--package-and-class-sort)
      (dolist (p packages)
        (goto-char (point-min))
        (when (and (re-search-forward (concat "^\\\\\\(usepackage\\|documentclass\\){"
                                              (regexp-quote p) "}") nil t)
                   (search-forward "%" (line-end-position) t))
          (setq result
                (append result
                        (split-string
                         (or (nth (if pkgs-instead 2 1)
                                  (split-string (thing-at-point 'line t) "%" nil (string ?\n)))
                             "") "," t)))))
      (cl-remove-duplicates result))))

(defun cmdlist--match-in-package-file (name &optional recurse file)
  "Return a list of all lines (minus final `%.*' in FILE (default `cmdlist-package-file)) which start with `\\usepackage' and include NAME in a comma-separated list after a `%'. If RECURSE is non-nil, also recursively return any lines which have a package after their second `%' which provides NAME."
  (unless file (setq file cmdlist-package-file))
  (with-temp-buffer
    (insert-file-contents file)
    (let ((res) ;; result
          (names (list name)) ;; List of all names we're looking for
          (pkgs-for-rec ()) ;; List of all packages we are recursively looking for
          (is-pkg nil) ;; Whether we're currently looking for a package.
          (case-fold-search nil)) ;; Unfortunate that this isn't the default
      (while (setq name (pop names))
        (while (re-search-forward
                (concat "^\\(\\\\usepackage{\\([^}]*\\)}\\)%"
                        (when is-pkg "[^%]*%")
                        "\\([^%]*,\\)?"
                        (regexp-quote name)
                        "\\([,%]\\|\n\\)")
                nil t)
          (push (match-string 1) res)
          (when (and recurse (not (member (match-string 2) pkgs-for-rec)))
            (push (match-string 2) names)
            (push (match-string 2) pkgs-for-rec)))
        (setq is-pkg t))
      res)))

(defun cmdlist--scan-for-defined-envs ()
  "Return a list of all environments defined by `\\\(re\)newenvironment's in the current buffer."
  (let ((res))
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\r?e?newenvironment" nil t)
        (push (car (split-string (cmdlist--shloop-latex-arg) nil nil "[{}]")) res)))
        (reverse res)))

(defun cmdlist--scan-for-packages ()
  "Return a list of all packages `\\usepackage'd in current buffer."
  (let ((res))
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\\\(usepackage\\|RequirePackage\\)" nil t)
        (while (not (eq (char-after) ?{)) (forward-sexp))
        (push (car (split-string (cmdlist--shloop-latex-arg) nil nil "[{}]")) res)))
    (reverse res)))

(defun cmdlist--get-document-class ()
  "Return a singleton list with the document class of this document or `nil'."
  (cmdlist--save-everything
    (goto-char (point-min))
    (when (search-forward "\\documentclass")
      (while (not (eq (char-after) ?{)) (forward-sexp))
      ;; list car is just to make it clearer what's happening
      (list (car (split-string (cmdlist--shloop-latex-arg) nil nil "[{}]"))))))

(defun cmdlist--singleton-or-prompt (l prompt)
  "L should be a list of strings. If L is a singleton, return it, otherwise prompt with PROMPT for an element to choose. If L is empty, return nil."
  ;; Adapted from `cmdlist--select-cmds-from-cmdlist'. Something should probably be factored out.
  (when l
    (if (= (length l) 1)
        (car l)
      (setq prompt (concat (cmdlist--list-of-things l) "\n" prompt))
      (let ((choice (nth
                     (- (cmdlist--prompt-for-number prompt 1 (length l)) 1)
                     l)))
        choice))))

(defun cmdlist--choose-and-add-package-or-class (cmd &optional package-file class pkgchoice)
  "Prompt for a package (or if CLASS, document class) from PACKAGE-FILE (default `cmdlist-package-file') and add cmd to it. Create a new package or class if necessary. Then sort the package file. Return a newline-terminated message explaining what happened. If PKGCHOICE is non-nil, use that as chosen package or document class instead of prompting."
  (unless package-file (setq package-file cmdlist-package-file))
  (let* ((definer (if class "\\documentclass" "\\usepackage"))
         (pkg-or-class-list
          (mapcar #'(lambda (x) (cmdlist--newcmd-name x definer))
                  (cmdlist--dofilter (x (cmdlist--read-lines package-file))
                    (string-prefix-p definer x))))
         (pkg
          (or pkgchoice
              (completing-read
               (concat "Choose a " (if class "document class" "package") ": ")
               pkg-or-class-list))))
    (with-temp-file package-file
      (when (file-exists-p package-file)
        (insert-file-contents package-file))
      (if (member pkg pkg-or-class-list)
          ;; We are adding to an existing line.
          (progn (goto-char (point-min))
                 (search-forward (concat "{" pkg "}"))
                 (beginning-of-line)
                 (re-search-forward "%\\([^%]*\\)\\(%\\|$\\)" (line-end-position))
                 (replace-match
                  (save-match-data
                    (mapconcat 'identity
                               ;; Warning! (sort) is destructive! (see above warning)
                               (seq-sort 'string<
                                         (append (split-string (match-string 1) ",") (list cmd)))
                               ","))
                  t t nil 1))
        ;; We are starting a new line.
        (goto-char (point-max))
        ;; In case the file is not newline-terminated as it should be
        (unless (or (eq (point) (point-min)) (eq (char-before) ?\n)) (insert "\n"))
        (insert (if class "\\documentclass{" "\\usepackage{") pkg "}%" cmd "\n"))
      (cmdlist--package-and-class-sort))
    (concat "`" cmd "' added to package `" pkg "'\n")))

(defun cmdlist--local-cmds-filename ()
  (concat "/tmp/" (replace-regexp-in-string "/" ":" (buffer-file-name)) ".tmp.commands"))

(defun cmdlist-test-command-in-minimal-file (&optional name)
  "Create and visit the file `cmdlist-test-minimal-file', and populate it with a minimal LaTeX file including the command which is prompted for (and defaults to the LaTeX command under point), so that you can test whether it is a built in command (or try to figure out what packages/document classes provide it."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "Command? " (cmdlist--latex-cmd-under-point))))
  (find-file cmdlist-test-minimal-file)
  (delete-region (point-min) (point-max))
  (insert
   "\\documentclass{minimal}\n"
   "\n"
   "\\begin{document}\n"
   "  \\" name "\n"
   "\\end{document}\n"))

(defun cmdlist--act-on-orphaned-command (c-or-e &optional prompt heading package-file builtin-file allow-edit-here)
  "Give the option of assign the given command or environment name to a package, or to add it to the list of builtin commands. Return a newline-terminated message saying what happened. If ALLOW-EDIT-HERE is non-nil, offer option to edit buffer at current position or to run `cmdlist-test-command-in-minimal-file'. In either case, `throw' the symbol `edit-here' with a cons pair, in which the second element is the current point, and the first element is the symbol `edit' or `c-or-e', respectively."
  (unless prompt (setq prompt ""))
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  ;; Prompt to add new builtin or add to a package
  ;; TODO: this should perhaps use read-multiple-choice; alternatively, cmdlist--replace-prompt should use something more like this
  (let ((decision
         (read-char (concat (propertize prompt 'face 'shadow)
                            (when (> (length prompt) 0)
                              (propertize "-------------------------------\n" 'face 'shadow))
                            "Command or environment `" c-or-e "' not found.\n"
                            "What to do?\n"
                            "(p) assign a package to it\n"
                            "(d) assign to current documentclass\n"
                            "(D) assign a documentclass to it\n"
                            "(b) add it as a builtin\n"
                            "(i) ignore it in this file for the rest of this session\n"
                            (when allow-edit-here "(t) test it in a minimal LaTeX file\n")
                            (when allow-edit-here "(e) edit the buffer here\n")
                            "(↵) do nothing"))))
    (cond
     ((and allow-edit-here (eq decision ?e))
      (throw 'edit-here (cons 'edit (point))))
     ((and allow-edit-here (eq decision ?t))
      (throw 'edit-here (cons c-or-e (point))))
     ((eq decision ?p)
      (cmdlist--choose-and-add-package-or-class c-or-e package-file nil))
     ((eq decision ?D)
      (cmdlist--choose-and-add-package-or-class c-or-e package-file t))
     ((eq decision ?d)
      (cmdlist--choose-and-add-package-or-class c-or-e package-file t
                                               (car (cmdlist--get-document-class))))
     ((eq decision ?i)
          (let ((fname (cmdlist--local-cmds-filename)))
            (with-temp-file fname
              (when (file-exists-p fname)
                (insert-file fname))
              ;; In case the file is not newline-terminated as it should be
              (unless (or (eq (point) (point-min)) (eq (char-before) ?\n)) (insert "\n"))
              (insert c-or-e "\n")))
          (concat "`" c-or-e "' marked temporarily as a file-local command\n"))
      ((eq decision ?b)
       (with-temp-file builtin-file
         (when (file-exists-p builtin-file)
           (insert-file-contents builtin-file))
         (goto-char (point-max))
         ;; In case the file is not newline-terminated as it should be
         (unless (or (eq (point) (point-min)) (eq (char-before) ?\n)) (insert "\n"))
         (insert c-or-e "\n")
         (let ((sort-fold-case t))
           (sort-lines nil (point-min) (point-max))))
       (concat "`" c-or-e "' added as builtin\n")))))

(defun cmdlist--scan-for-other-defined-cmds ()
  "Return a list of all commands defined in the current buffer using commands in `cmdlist-cmd-defining-cmds'."
  ;; This is adapted from cmdlist--scan-for-newcmds. Perhaps something should be factored out?
  (let* ((res)
         (cmds (mapcar (lambda (x) (if (stringp x) x (car x))) cmdlist-cmd-defining-cmds))
         (regx (concat "\\\\\\(" (mapconcat 'regexp-quote cmds "\\|") "\\)[^a-z]")))
    (cmdlist--save-everything
      (goto-char (point-min))
      (while (re-search-forward regx nil t)
        (let ((match (match-string-no-properties 1))
              (defined-cmd
                (progn
                  ;; Take back the "look-ahead" in the regex
                  (backward-char)
                  ;; Remove any surrounding brackets and backslashes
                  (car (split-string (cmdlist--shloop-latex-arg) nil nil "\\\\?[{}]*")))))
          (if (member match cmdlist-cmd-defining-cmds)
              (push defined-cmd res)
            (let ((generated-cmds
                   (funcall (nth 1 (car (cl-member-if (lambda (x)
                                                     (and (listp x)
                                                          (string= (car x) match)))
                                                   cmdlist-cmd-defining-cmds)))
                            defined-cmd)))
              (dolist (c generated-cmds) (push c res)))))))
    (reverse res)))

(defun cmdlist--buffer-provided-cmds (&optional package-file builtin-file)
  "Get all commands which are defined or provided for in this buffer."
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (append (cmdlist--read-lines builtin-file)
          ;; TODO: This is wasteful. These should be combined into a single command.
          (mapcar 'cmdlist--newcmd-name (cmdlist--scan-for-newcmds))
          (mapcar #'(lambda (x) (cmdlist--newcmd-name x "\\newtheorem\\*?")) (cmdlist--scan-for-newthms))
          (cmdlist--scan-for-other-defined-cmds)
          (cmdlist--scan-for-defined-envs)
          (when (file-exists-p package-file)
            (cmdlist--scan-package-file (append (cmdlist--scan-for-packages) (cmdlist--get-document-class)) t package-file))
          (cmdlist--read-lines (cmdlist--local-cmds-filename))))

(defun cmdlist-package-update-latex-buffer (&optional heading package-file builtin-file)
  "Add to current buffer all ``\\usepackage's defined in PACKAGE-FILE (default is `cmdlist-package-file') under HEADING (default is `cmdlist-package-heading') which provide commands or environments present in the current file and not built-in (according to BUITLIN-FILE, default `cmdlist-builtin-file') or already defined. Prompt to act on any unmatched commands or enviornments."
  (interactive)
  ;; This is adapted from `cmdlist-update-latex-buffer'. Perhaps something should be factored out?
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (let ((cmds-and-envs (append (cmdlist--scan-for-latex-cmds) (cmdlist--scan-for-latex-envs)))
        (exceptions (cmdlist--buffer-provided-cmds package-file builtin-file))
        (curex "")
        (lastmessage ""))
    ;; Warning! (sort) is destructive! (see above warning)
    (setq cmds-and-envs (cl-remove-duplicates (seq-sort 'string< cmds-and-envs)))
    (setq exceptions (cl-remove-duplicates (seq-sort 'string< exceptions)))
    ;; This catches the `edit-here' possibly thrown by `cmdlist--act-on-orphaned-command' and goes to the point thrown by that `throw'
    (let ((edit-point
           (catch 'edit-here
             (dolist (c-or-e cmds-and-envs)
               ;; Check if c-or-e is an exception or has an @-sign
               (unless (or (and cmdlist-ignore-at-symbol (member ?@ (string-to-list c-or-e)))
                           (progn
                             (while (and (string< curex c-or-e)
                                         exceptions)
                               (setq curex (pop exceptions)))
                             (string= curex c-or-e)))
                 ;; Find a matching package
                 (let ((pkgmatch (cmdlist--singleton-or-prompt
                                  (cmdlist--match-in-package-file c-or-e t package-file)
                                  (concat "\n Multiple packages provide `"
                                          c-or-e "'. Choose one: "))))
                   ;; If we found one, add it, and refresh exceptions.
                   (if pkgmatch
                       (progn
                         (cmdlist--stick-at-top heading pkgmatch)
                         (setq exceptions (cmdlist--buffer-provided-cmds package-file builtin-file))
                         ;; Warning! (sort) is destructive! (see above warning)
                         (setq exceptions (cl-remove-duplicates (seq-sort 'string< exceptions)))
                         (setq curex ""))
                     ;; Otherwise, act on it
                     (cmdlist--save-everything
                       ;; Display the command in question in the buffer
                       (goto-char (point-min))
                       (let ((case-fold-search nil))
                         (re-search-forward (concat "\\\\\\(begin{\\)?{?"
                                                    (regexp-quote c-or-e)
                                                    "\\($\\|[^a-zA-Z]\\)"))
                         (goto-char (match-beginning 0)))
                       (setq lastmessage
                             (cmdlist--act-on-orphaned-command
                              c-or-e lastmessage heading package-file builtin-file t))
                       ;; This is so catch only returns non-nil in the case of a throw
                       nil))))))))
      (when edit-point
        (push-mark)
        (when (member 'evil features)
          (evil-set-jump))
        (goto-char (cdr edit-point))
        (unless (eq (car edit-point) 'edit)
          (cmdlist-test-command-in-minimal-file (car edit-point)))))))

(defun cmdlist-package-handle-command (&optional c-or-e heading package-file builtin-file)
  "Like `cmdlist-package-update-latex-buffer', but only do it for given command. If C-OR-E is nil, prompt for command, defaulting to command at point."
  ;; Some of this should probably be factored out too.
  (interactive)
  (unless c-or-e
    (setq c-or-e (read-from-minibuffer "Name of command or environment to handle? \\" (cmdlist--latex-cmd-under-point)))
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (let ((exceptions (cmdlist--buffer-provided-cmds package-file builtin-file)))
    (if (member c-or-e exceptions)
        ;; Nothing to do
        (message "%s" (concat "`" c-or-e "' already defined or provided"))
      ;; Find a matching package
      (let ((pkgmatch (cmdlist--singleton-or-prompt
                       (cmdlist--match-in-package-file c-or-e t package-file)
                       (concat "\n Multiple packages provide `" c-or-e "'. Choose one: "))))
        ;; If we found one, add it
        (if pkgmatch
            (cmdlist--stick-at-top heading pkgmatch)
          (message "%s" (cmdlist--act-on-orphaned-command
                         c-or-e nil heading package-file builtin-file nil))))))))

(provide 'cmdlist)

;;; cmdlist.el ends here
