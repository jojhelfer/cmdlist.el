;;; cmdlist.el --- Automated latex command maintenance

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

;; There are several convenience functions, most of which are listed in
;; the example keybindings below. The most import one is
;; cmdlist-update-latex-buffer. See the documentation of those
;; functions for more info, as well as the customizatoin variables
;; below under "Variables".

;; Also see README.md for more information.

;; My keybdindings:

;; (evil-define-key 'normal LaTeX-mode-map
;;   (kbd "SPC g u") 'cmdlist-update-latex-buffer
;;   (kbd "SPC g c") 'generate-and-add-cmd
;;   (kbd "SPC g a") 'add-cmd-to-file
;;   (kbd "SPC g r") 'generate-mathrm
;;   (kbd "SPC g b") 'generate-mathbf
;;   (kbd "SPC g B") 'generate-mathbb
;;   (kbd "SPC g C") 'generate-mathcal
;;   (kbd "SPC g F") 'generate-mathfrak
;;   (kbd "SPC g o") 'generate-operatorname
;;   (kbd "SPC g p") 'generate-package
;;   (kbd "SPC g z") 'delete-unused-newcmds
;;   (kbd "SPC g f") 'open-cmdlist-file)

;; TODO Look into just parsing the whole file, hopefully with some pre-existing tool. In particular, look into AucTeX's TeX-auto-file, TeX-auto-save, etc.
;; TODO Adapt sort-newcmds for \\usepackage (or maybe not since we are not sorting packages now)
;; TODO Update documentation for theorem and package handling
;; TODO Include compile-update-and-save helper functions
;; TODO Maybe handle commands with non [a-zA-Z] characters (like @ and *) in the name
;; TODO Add commands to clean unused theorems and packages
;; TODO Try to guess command provider by looking through package/class files
;; TODO Allow annotating usepackage with provided commands (and adding to these automatically?)
;; TODO Create temporary latex file for testing commands to see where they come from
;; TODO Ignore commands used in comments
;; TODO Be careful about commands which are the empty string
;; TODO When jumping to the instance of a command during querying, it should be case-sensitive.

;; We use some cl stuff
(require 'cl)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar cmdlist-files '("~/.latex-commands.sty")
  "List of files containing `\\\(re\)newcommand' entires to be used by `cmdlist.el'. The first entry is also used to store new commands.")

(defvar cmdlist-heading "% Commands"
  "Heading under which commands are inserted into the current buffer.")

(defvar cmdlist-visit-after-adding 'nil
  "If non-nil, visit cmdlist file after adding new command, unless value is `ask', in which case ask first.")

(defvar cmdlist-also-add-to-buffer nil
  "If non-nil, also add command to buffer when adding to cmdlist, unless value is `ask', in which case ask first.")

(defvar cmdlist-add-to-file-default nil
  "If non-nil, add new commands to cmdlist file by default (i.e., without prefix argument) rather than to current buffer.")

(defvar cmdlist-braces-around-cmd-name nil
  "If non-nil, put braces around the command name (as in `\newcommand{\foo}' when adding new commands.")

(defvar cmdlist-theorem-file "~/.latex-theorems.sty"
  "Files containing `\\newtheorem' entires to be used by `cmdlist'.")

(defvar cmdlist-default-shared-counter "defn"
  "Default shared counter for `newtheorem's.")

(defvar cmdlist-default-parent-counter nil
  "Default parent counter for `newtheorem's (if `cmdlist-default-shared-counter' is `nil').")

(defvar cmdlist-package-file "~/.latex-packages.sty"
  "File each of whose lines contains a `\\usepackage' or `\\documentclass' command followed by a comment with a comma-separated list of commands and environment it provides.")

(defvar cmdlist-package-heading "% Packages"
  "Heading under which packages are inserted into the current buffer.")

(defvar cmdlist-builtin-file "~/.latex-builtins"
  "File containing a list of built-in latex commands and environments, one per line.")

(defvar cmdlist-cmd-defining-cmds
  (list "let" "def"
        (list "newif"
              (lambda (cmd)
                ;; Remove starting backslash if its there
                (when (string-prefix-p "\\" cmd) (setq cmd (substring cmd 1)))
                (append (list cmd)
                        (when (>= (length cmd) 2)
                          (let ((cmdnoif (substring cmd 2)))
                            (list cmd (concat cmdnoif "true") (concat cmdnoif "false"))))))))
  "List of commands (besides `\(re\)newcommand' and `newtheorem') which define a new command (with a backslash). Each entry also be a list (cmd fun) where cmd is the name of the command, nobs indicates the arugment of cmd does not have a backslash, and fun takes its argument and returns a list of commands it defines.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dofilter (varbind &rest body)
  "Binds VAR to each element of LIST and performs BODY, returning the list of those elements for which result of BODY is non-nil.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  (let ((res (make-symbol "")))
    `(let ((,res))
       (dolist ,varbind
         (if (progn ,@body)
             (push ,(nth 0 varbind) ,res)))
       (reverse ,res))))

(defun zip1 (&rest args)
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

(defun zip (&rest args)
  (let ((res)
        (cur (apply 'zip1 args)))
    (while cur
      (push (car cur) res)
      (setq cur (apply 'zip1 (cdr cur))))
    (reverse res)))

(defun list-of-things (things &optional fmt)
  "Concatenates the strings in THINGS, separated by newlines with a number at the beginning of each line.
FMT specifies how the number should be formatted (default \"[%d]\")."
  (unless fmt
      (setq fmt "[%d] "))
  (let* ((nums (mapcar (lambda (x) (format fmt x)) (number-sequence 1 (length things))))
         (lines (zip nums things)))
    (mapconcat (lambda (x) (apply 'concat x)) lines "\n")))

(defun prompt-for-number (prompt &optional min max)
  "Keep displaying PROMPT and asking the user for input until the input is an integer (between MIN and MAX, if provided), then return it as an integer."
  (let ((response "")
        (res 0))
    (while (or (and min (< res min))
               (and max (> res max))
               (not (equal response (format "%d" res))))
      (setq response (read-from-minibuffer prompt))
      (setq res (string-to-number response)))
  res))

(defun forward-brexp ()
  "Move forward across one matching curly bracket expression."
  (interactive)
  (when (search-forward "{" nil nil)
    (let ((count 1))
      (while (> count 0)
        (re-search-forward "[{}]")
        (unless (eq (char-before (- (point) 1)) ?\\)
          (setq count (+ count
                         (if (eq (char-before) ?\{) 1 -1))))))))

(defmacro save-everything (&rest body)
  "Save mark-and-excursion, restriction, and match-data."
  (declare (indent 0) (debug t))
  `(save-mark-and-excursion
     (save-restriction
       (save-match-data
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;
;; Parsing commands ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun shloop-latex-arg ()
  "Move past the latex argument (bracket expression, command name, or single character) starting under point, and return it"
  ;; Move past any spaces and comments
  (while
      (cond ((eq (char-after) ? ) (forward-char) t))
    (cond ((eq (char-after) ?%) (forward-line) t)))
  (let ((start (point)))
    (cond
     ((eq (char-after) ?\{) (forward-brexp))
     ((eq (char-after) ?\\)
      (forward-char)
      (re-search-forward "[^A-Za-z]")
      (backward-char))
     ((forward-char)))
    (buffer-substring-no-properties start (point))))

(defun latex-cmd-under-point ()
  "If point is on a latex command, return its name, else nil"
  (let ((start (point)))
    (save-everything
      (when (< (point) (point-max))
        (forward-char))
      (when (search-backward "\\" nil t)
        (let ((cmd (shloop-latex-arg)))
          (when (<= start (point))
            (substring cmd 1)))))))

(defun search-backward-incl (string)
  "Goto beginning of first instance STRING occurring before or around point. Return nil if STRING was not found."
  (let ((len (length string))
        (start))
    (save-everything
      (forward-char)
      (search-backward (substring string 0 1) nil t)
      (when (or (and (<= (+ (point) len) (point-max))
                     (equal string (buffer-substring-no-properties (point) (+ (point) len))))
                (search-backward string nil t))
        (setq start (point))))
    (when start
      (goto-char start))))

(defun re-search-backward-incl (regex)
  "Goto beginning of first instance REGEX occurring before or around point. Return nil if STRING was not found."
  (let ((start (point))
        (place))
    (save-everything
      (if (re-search-backward regex nil t)
          (progn (setq place (match-beginning 0))
                 (forward-char))
        (goto-char (point-min)))
      (when (and (re-search-forward regex nil t)
                 (<= (match-beginning 0) start))
        (setq place (match-beginning 0))))
    (when place
      (goto-char place))))

(defun surrounding-newcmd (&optional regex)
  "If point is not inside of a latex `\\\(re\)newcommand' (or given REGEX, e.g., `\\newtheorem'), return nil. Otherwise, return the text of the whole command."
  (unless regex (setq regex "\\\\r?e?newcommand"))
  (let ((start (point))
        (beg))
    (save-everything
      (when (re-search-backward-incl regex)
        (setq beg (point))
        (search-forward-regexp regex)
        (when (eq (char-after) ?\{)
          (forward-brexp))
        (forward-brexp)
        (when (>= (point) start)
          (end-of-line)
          (buffer-substring-no-properties beg (point)))))))

(defun scan-for-latex-cmds (&optional ignore-newcmds)
  "Return all names of latex commands in current buffer."
  (let (cmds last-newcmd-pos)
    (save-everything
      (when ignore-newcmds
        (goto-char (point-max))
        (if (not (search-backward "\\newcommand" nil t))
            (setq ignore-newcmds nil)
          (forward-char 11)
          (when (eq (char-after) ?\{)
            (forward-brexp))
          (forward-brexp)
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
                   (evenp (- end beg))))
          (let ((cmd (latex-cmd-under-point)))
            (unless (or (not cmd)
                        (equal cmd "")
                        (and ignore-newcmds
                             (< (point) last-newcmd-pos)
                             (let ((snc (surrounding-newcmd)))
                               (and snc
                                    (equal cmd (newcmd-name snc))))))
              (push cmd cmds)))))
    (reverse (delete-dups cmds)))))

(defun newcmd-name (cmd)
  "Return the name of the given `\\\(re\)newcommmand' or `\\newtheorem'."
  (save-everything
    (with-temp-buffer
      (insert cmd)
      (goto-char (point-min))
      (re-search-forward "\\\\r?e?newcommand\\|\\\\newtheorem")
      (car (split-string (shloop-latex-arg) nil nil "[{}]*\\\\?")))))

(defun scan-for-newcmds ()
  "Return a list of all \\newcommmands in the current buffer"
  (let ((res))
    (save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\r?e?newcommand" nil t)
        (push (surrounding-newcmd) res)
        (forward-brexp)))
    (reverse res)))

(defun scan-file-for-newcmds (file)
  "Return a list of all \\newcommmands in the given file"
  (save-mark-and-excursion
    (with-temp-buffer
      (insert-file-contents file)
      (scan-for-newcmds))))

(defun assemble-newcmd (name defn &optional numargs opt)
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

(defun num-args-in-defn (defn)
  "Return the greatest argument number in the latex command definition DEFN. Only work for definitions with at most 9 arguments."
  (let ((maxnum 0))
    (save-everything
      (with-temp-buffer
        (insert defn)
        (goto-char (point-min))
        (while (search-forward "#" nil t)
          (let ((newnum (string-to-number (char-to-string (char-after)))))
            (when (> newnum maxnum)
              (setq maxnum newnum))))))
    maxnum))

(defun sort-newcmds (reverse beg end)
  "Variant of `sort-lines' (using the amazingly flexible `sort-subr') which keeps each (possibly multi-line) `\\newcommand' together, along with any lines before it up to the previous `\\newcommand'. Also ignores case."
  (interactive "P\nr")
  (save-everything
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ;; To make `end-of-line' and etc. to ignore fields.
        ((inhibit-field-text-motion t)
         (sort-fold-case t))
      (sort-subr reverse 'forward-line
                 (lambda ()
                   (re-search-backward-incl "\\\\r?e?newcommand")
                   (re-search-forward "\\\\r?e?newcommand")
                   (when (eq (char-after) ?\{) (forward-brexp))
                   (forward-brexp))
                 (lambda () (re-search-forward "\\\\r?e?newcommand{?") nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding commands in a list of \newcommands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cmdlist-matches (cmdlist name)
  "Return all elements of CMDLIST which are `\\\(re\)newcommand's defining the command NAME."
  (dofilter (cmd cmdlist)
    (or
     (string-prefix-p (concat "\\newcommand{\\" name "}") cmd)
     (string-prefix-p (concat "\\newcommand\\" name "[") cmd)
     (string-prefix-p (concat "\\newcommand\\" name "{") cmd)
     (string-prefix-p (concat "\\renewcommand{\\" name "}") cmd)
     (string-prefix-p (concat "\\renewcommand\\" name "[") cmd)
     (string-prefix-p (concat "\\renewcommand\\" name "{") cmd))))

(defun select-cmds-from-cmdlist (cmdlist names exceptions)
  "For each element of NAMES which is not in EXCEPTION, get all elements of CMDLIST which are `\\newcommand's defining this element, and return them in a list. Each time there are multiple matches, the user is queried for a choice."
  (let ((choices))
    (dolist (name names)
      (unless (member name exceptions)
        (let ((cmds (get-cmdlist-matches cmdlist name))
              (cmd))
          (if (> (length cmds) 1)
              (let ((prompt ""))
                (setq prompt (list-of-things cmds))
                (setq prompt (concat prompt "\nMultiple entires for " name ". Choose from above: "))
                (setq cmd (nth
                           (- (prompt-for-number prompt 1 (length cmds)) 1)
                           cmds)))
            (when cmds
              (setq cmd (nth 0 cmds))))
          (when cmd
            (push cmd choices)))))
    choices))

(defun cmdlist-already-p (cmdlist entry)
  "If the command defined in ENTRY is in cmdlist, display all matches and ask the user to proceed."
  (let* ((name (newcmd-name entry))
         (matches (get-cmdlist-matches cmdlist name))
         (nmatches (length matches)))
    (if (not matches) t
      (let ((prompt))
        (setq prompt (list-of-things matches))
        (setq prompt (concat prompt
                             (concat "\n" name " already defined. Continue anyway? ")))
        (y-or-n-p prompt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying buffer and adding new commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stick-at-top (heading text &optional sortfun)
  "Stick TEXT (or each string in TEXT) after the first occurrence of HEADING, then sort it using SORTFUN if non-nil.
If HEADING does not occur, first insert HEADING before \\begin{document}"
  (let ((starting-point))
    (save-everything
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

(defun add-to-cmdlist-file (newcmd &optional file)
  "Add NEWCMD to FILE (default: the first entry in `cmdlist-files') and then sort the lines.

If the command being defined in NEWCMD is already in FILE, ask for confirmation."
  (unless file (setq file (car cmdlist-files)))
  (if (and (file-readable-p file) (file-writable-p file))
      (let ((cmdlist (scan-file-for-newcmds file)))
        (if (cmdlist-already-p cmdlist newcmd)
            (save-everything
              (let ((fibuf (find-file-noselect file)))
                (with-current-buffer fibuf
                  (goto-char (point-min))
                  (insert newcmd)
                  (insert "\n")
                  (sort-newcmds nil (point-min) (point-max))
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
          (message "Operation cancelled.")))
    (message "File %s does not exist or not readable/writable." file)))

(defun add-newcmd (newcmd &optional heading file)
  "Add the given new command to the top of the file under HEADING (default: `cmdlist-heading'), asking for confirmation if already defined.

If FILE is given, instead add it to FILE. If FILE is `t', it is set to the first element of `cmdlist-files'."
  (if file
      (progn
        (unless (stringp file) (setq file (car cmdlist-files)))
        (add-to-cmdlist-file newcmd file))
    (unless heading (setq heading cmdlist-heading))
    (if (cmdlist-already-p (scan-for-newcmds) newcmd)
        (progn
          (stick-at-top heading newcmd 'sort-newcmds)
          (message "New entry\n  %s\nadded under %s ." newcmd heading))
      (message "Operation cancelled"))))

(defun generate-newcmd ()
  "Generate a `\\newcommand' and return it. The name and definition of the new macro are queried for.

The number of arguments is guessed by how many are used in the command. If there is at least one, query for an optional argument.

If there is a macro under the current point, the default name is
this macro."
  (let* ((name (latex-cmd-under-point))
         (defn)
         (numargs)
         (optional))
    (if (not name)
        (setq name ""))
    (setq name (read-from-minibuffer "Name of new command? \\" name))
    (setq defn (read-from-minibuffer (concat "Definition of command \\" name " ? ")))
    (setq numargs (num-args-in-defn defn))
    (when (and (> numargs 0)
               (y-or-n-p "Optional argument? "))
      (setq optional
            (read-from-minibuffer (format "Deafult optional argument for \\%s[%d]? "
                                          name numargs))))
    (assemble-newcmd name defn numargs optional)))

(defun cmdlist-update-latex-buffer (&optional heading files prefix)
  "Add to current buffer under HEADING all commands defined in FILES which are present in the current file and not already defined. If PREFIX is non-nil, it is prepended to `\\newcommand'.

By default HEADING is `cmdlist-heading' and FILES are the files in the variable `cmdlist-files'."
  (interactive)
  (unless heading (setq heading cmdlist-heading))
  (unless files (setq files cmdlist-files))
  (unless prefix (setq prefix ""))
  (let* ((cmdlist (apply 'append (mapcar 'scan-file-for-newcmds files)))
         (cmds (scan-for-latex-cmds))
         (exceptions (mapcar 'newcmd-name (scan-for-newcmds)))
         (newcmds (select-cmds-from-cmdlist cmdlist cmds exceptions)))
    (if newcmds
        (progn
          (setq newcmds (mapcar (lambda (x) (concat prefix x)) newcmds))
          (stick-at-top heading newcmds 'sort-newcmds)
          (message "Added %d new commands:\n%s" (length newcmds) (list-of-things newcmds)))
      (message "No commands added."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-cmd-to-file ()
  "Add the `\\newcommand' definition under point to the first entry of `cmdlist-files'."
  (interactive)
  (let ((newcmd (surrounding-newcmd)))
    (when newcmd
      (add-newcmd (surrounding-newcmd) nil t))))

(defun generate-and-add-cmd (tofile)
    "Generate a `\\newcommand' with `generate-newcmd' and add it to current file under `cmdlist-heading'. With or without prefix argument (depending on `cmdlist-add-to-file-default'), add to first entry of `cmdlist-files' instead."
    (interactive "P")
    (when cmdlist-add-to-file-default
      (setq tofile (not tofile)))
    (let* ((newcmd (generate-newcmd))
           (tobuf (and tofile
                       cmdlist-also-add-to-buffer
                       (if (eq cmdlist-also-add-to-buffer 'ask)
                           (y-or-n-p (format "Also add\n  %s\nto buffer?" newcmd)) t))))
      (add-newcmd newcmd nil (when tofile t))
      (when tobuf
        (add-newcmd newcmd nil nil))))

(defun generate-and-add-fontletter (font &optional heading file)
  "Generate a ``\\newcommand'' of the form `\\newcomand\\name{\\FONT{letter}}' and add it to the current buffer or to FILE with `add-newcmd' (hence FILE can be `t').

The name and letter are queried for, and by default are both the latex macro under point."
  (let* ((name (read-from-minibuffer
                (format "Name of %s command? \\" font) (latex-cmd-under-point)))
         (letter (read-from-minibuffer "Text? " name)))
    (add-newcmd (assemble-newcmd name (format "\\%s{%s}" font letter)) heading file)))

(defun generate-mathbb (tofile)
  "Run `generate-and-add-fontletter' with `mathbb'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "mathbb" nil (when tofile t)))

(defun generate-mathbf (tofile)
  "Run `generate-and-add-fontletter' with `mathbf'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "mathbf" nil (when tofile t)))

(defun generate-mathrm (tofile)
  "Run `generate-and-add-fontletter' with `mathrm'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "mathrm" nil (when tofile t)))

(defun generate-mathcal (tofile)
  "Run `generate-and-add-fontletter' with `mathcal'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "mathcal" nil (when tofile t)))

(defun generate-mathfrak (tofile)
  "Run `generate-and-add-fontletter' with `mathfrak'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "mathfrak" nil (when tofile t)))

(defun generate-operatorname (tofile)
  "Run `generate-and-add-fontletter' with `operatorname'. Prefix argument puts it in the cmdlist file."
  (interactive "P")
  (generate-and-add-fontletter "operatorname" nil (when tofile t)))

(defun generate-package ()
  "Ask for a package name and stick it under `% Packages'"
  (interactive)
  (let ((name (read-from-minibuffer "Package name? ")))
    (stick-at-top "% Packages" (concat "\\usepackage{" name "}"))))

(defun get-unused-newcmds ()
  "Return a list of `\\newcommand's in this buffer which are (apparently) not being used."
  (let* ((cmds (scan-for-latex-cmds t))
         (newcmds (scan-for-newcmds))
         (unuseds (dofilter (x newcmds)
                    (not (member (newcmd-name x) cmds)))))
    unuseds))

(defun show-unused-newcmds ()
  "Display `\\newcommand's in this buffer which are (apparently) not being used."
  (interactive)
  (let ((unuseds (get-unused-newcmds)))
    (if unuseds
        (message "Seemingly unused commands:\n%s" (list-of-things unuseds))
      (message "No unused commands found"))))

(defun delete-unused-newcmds ()
  "Delete (seemingly) unused `\\\(re\)newcommand's and `\\\\newtheorem's in this buffer, after prompting."
  (interactive)
  (let ((unuseds (append (get-unused-newcmds) (get-unused-newthms))))
    (if unuseds
        (when (y-or-n-p (format "Seemingly unused commands:\n%s\nDelete? " (list-of-things unuseds)))
          (dolist (x unuseds)
            (save-everything
              (goto-char (point-min))
              (search-forward x)
              (beginning-of-line)
              (kill-line 1))))
      (message "No unused commands found"))))

(defun open-cmdlist-file ()
  "`(find-file-other-window (car cmdlist-files))'"
  (interactive)
  (let ((cmd (latex-cmd-under-point)))
    (find-file-other-window (car cmdlist-files))
    ;; Use swiper if possible
    (if (require 'swiper nil t)
        (swiper)
      (setq cmd (completing-read "Goto command: "
                                 (mapcar 'newcmd-name (scan-for-newcmds))
                                 nil nil nil nil cmd))
      (when cmd
        (let ((cmd-pos))
          (save-everything
            (goto-char (point-min))
            (when (re-search-forward (format "\\\\r?e?newcommand{?\\\\%s[}{\\[]" cmd) nil t)
              (setq cmd-pos (point))))
          (when cmd-pos
            (goto-char cmd-pos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments and theorems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scan-for-latex-envs ()
  "Return all names of latex environments in current buffer."
  (let (envs)
    (save-everything
      (goto-char (point-min))
      (while (search-forward "\\begin{" nil t)
        (backward-char)
        (push (car (split-string (shloop-latex-arg) nil nil "[{}]")) envs)))
    (reverse (delete-dups envs))))

(defun stick-thm-at-top (text &optional style shared-counter parent-counter)
  "Run stick-at-top with TEXT and with heading set to `\theoremstyle{STYLE}' (by default, STYLE is  read from a comment at the end of text, which is removed, and is otherwise `plain'), and with no sorting. If SHARED-COUNTER is provided, add it as an optional argument after the first argument. Otherwise, if PARENT-COUNTER is provided, add it as an optional argument after the second argument. Otherwise, if `cmdlist-default-shared-counter' is non-nil, use that as SHARED-COUNTER. Otherwise, if `cmdlist-default-parent-counter' is non-nil, use that as PARENT-COUNTER."
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
  (stick-at-top (concat "\\theoremstyle{" style "}") text))

(defun scan-for-newthms ()
  "Return a list of all `\\newtheorem's in the current buffer"
  ;; This is adapted from scan-for-newcmds. Perhaps something should be factored out?
  (let ((res))
    (save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\newtheorem" nil t)
        (push (surrounding-newcmd "\\\\newtheorem") res)
        (forward-brexp)))
    (reverse res)))

(defun get-unused-newthms ()
  "Return a list of `\\newtheorem's in this buffer which are (apparently) not being used."
  ;; This is adapted from `get-unused-newthms'. Perhaps something should be factored out?
  (let* ((envs (scan-for-latex-envs))
         (newthms (scan-for-newthms))
         (unuseds (dofilter (x newthms)
                    (not (member (newcmd-name x) envs)))))
    unuseds))

(defun scan-file-for-newthms (file)
  "Return a list of all `\\newtheorem's in the given file"
  ;; This is adapted from `scan-file-for-newcmds'. Perhaps something should be factored out?
  (save-mark-and-excursion
    (with-temp-buffer
      (insert-file-contents file)
      (scan-for-newthms))))

(defun cmdlist-newthm-update-latex-buffer (&optional file)
  "Add to current buffer all ``\\newtheorem's defined in FILE (using `stick-thm-at-top') which are present in the current file and not already defined. By default FILE is `cmdlist-theorem-file'."
  ;; This is adapted from `cmdlist-update-latex-buffer'. Perhaps something should be factored out?
  (interactive)
  (unless file (setq file cmdlist-theorem-file))
  (let* ((thmlist (scan-file-for-newthms file))
         (envs (scan-for-latex-envs))
         (exceptions (mapcar 'newcmd-name (scan-for-newthms)))
         (newthms
          (dofilter (thm thmlist)
            (and (member (newcmd-name thm) envs)
                 (not (member (newcmd-name thm) exceptions))))))
    (if newthms
        (progn
          (dolist (thm newthms)
            (stick-thm-at-top thm))
          (message "Added %d new theorem:\n%s" (length newthms) (list-of-things newthms)))
      (message "No theorems added."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages and built-ins ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun package-and-class-sort ()
  "Sort lines in buffer ignoring initial `\\usepackage' or `\\documentclass'."
  (save-everything
    (goto-char (point-min))
    (let ((sort-fold-case t))
      (sort-subr nil 'forward-line 'end-of-line
                 (lambda ()
                   (save-match-data
                     (and (re-search-forward
                           "^\\\\\\(usepackage\\|documentclass\\)" nil t) nil)))))))

(defun scan-package-file (packages &optional recurse file pkgs-instead)
  "Return a list of all comma-separated entries appearing after `%'s in FILE (default is `cmdlist-package-file') in lines defining packages in PACKAGES. If RECURSE is non-nil, also recursively include entries appearing in packages appearing after the second `%' in some line. If PKGS-INSTEAD is non-nil, return the entries after the second `%' instead of the first."
  (unless file (setq file cmdlist-package-file))
  ;; If we're recursing, add all packages.
  (when recurse
    (let ((new-pkgs packages))
      (while new-pkgs
        (let ((newer-pkgs ()))
          (dolist (p (scan-package-file new-pkgs nil file t))
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
      (package-and-class-sort)
      (goto-char (point-min))
      (dolist (p packages)
        (when (and (re-search-forward (concat "^\\\\\\(usepackage\\|documentclass\\){"
                                              (regexp-quote p) "}") nil t)
                   (search-forward "%" (line-end-position) t))
          (setq result
                (append result
                        (split-string
                         (or (nth (if pkgs-instead 2 1)
                                  (split-string (thing-at-point 'line t) "%" nil (string ?\n)))
                             "") "," t)))))
      (remove-duplicates result))))

(defun match-in-package-file (name &optional recurse file)
  "Return a list of all lines (minus final `%.*' in FILE (default `cmdlist-package-file)) which start with `\\usepackage' and include NAME in a comma-separated list after a `%'. If RECURSE is non-nil, also recursively return any lines which have a package after their second `%' which provides NAME."
  (unless file (setq file cmdlist-package-file))
  (with-temp-buffer
    (insert-file-contents file)
    (let ((res) ;; result
          (names (list name)) ;; List of all names we're looking for
          (pkgs-for-rec ()) ;; List of all packages we are recursively looking for
          (is-pkg nil)) ;; Whether we're currently looking for a package.
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

(defun read-lines (file)
  "Read lines of file into a list and return it, or nil if file does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t))))

(defun scan-for-defined-envs ()
  "Return a list of all environments defined by `\\\(re\)newenvironment's in the current buffer."
  (let ((res))
    (save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\r?e?newenvironment" nil t)
        (push (car (split-string (shloop-latex-arg) nil nil "[{}]")) res)))
        (reverse res)))

(defun scan-for-packages ()
  "Return a list of all packages `\\usepackage'd in current buffer."
  (let ((res))
    (save-everything
      (goto-char (point-min))
      (while (re-search-forward "\\\\usepackage" nil t)
        (while (not (eq (char-after) ?{)) (forward-sexp))
        (push (car (split-string (shloop-latex-arg) nil nil "[{}]")) res)))
    (reverse res)))

(defun get-document-class ()
  "Return a singleton list with the document class of this document or `nil'."
  (save-everything
    (goto-char (point-min))
    (when (search-forward "\\documentclass")
      (while (not (eq (char-after) ?{)) (forward-sexp))
      ;; list car is just to make it clearer what's happening
      (list (car (split-string (shloop-latex-arg) nil nil "[{}]"))))))

(defun singleton-or-prompt (l prompt)
  "L should be a list of strings. If L is a singleton, return it, otherwise prompt with PROMPT for an element to choose. If L is empty, return nil."
  ;; Adapted from `select-cmds-from-cmdlist'. Something should probably be factored out.
  (when l
    (if (= (length l) 1)
        (car l)
      (setq prompt (concat (list-of-things l) "\n" prompt))
      (let ((choice (nth
                     (- (prompt-for-number prompt 1 (length l)) 1)
                     l)))
        choice))))

(defun choose-and-add-package-or-class (cmd &optional package-file class pkgchoice)
  "Prompt for a package (or if CLASS, document class) from PACKAGE-FILE and add cmd to it. Create a new package or class if necessary. Then sort the package file. Return a newline-terminated message explaining what happened. If PKGCHOICE is non-nil, use that as chosen package or document class instead of prompting."
  (unless package-file (setq package-file cmdlist-package-file))
  (let ((pkg
         (or pkgchoice
             (completing-read
              (concat "Choose a " (if class "document class" "package") ": ")
              (dofilter (x (read-lines package-file))
                (or
                 (and (not class) (string-prefix-p "\\usepackage" x))
                 (and class (string-prefix-p "\\documentclass" x))))))))
    (with-temp-file package-file
      (when (file-exists-p package-file)
        (insert-file-contents package-file))
      (if (or (string-prefix-p "\\usepackage" pkg)
              (string-prefix-p "\\documentclass" pkg))
          ;; We are adding to an existing line.
          (progn (goto-char (point-min))
                 (search-forward pkg)
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
      (package-and-class-sort))
    (concat "`" cmd "' added to package `" pkg "'\n")))

(defun local-cmds-filename ()
  (concat "/tmp/" (replace-regexp-in-string "/" ":" (buffer-file-name)) ".tmp.commands"))

(defun act-on-orphaned-command (c-or-e &optional prompt heading package-file builtin-file allow-edit-here)
  "Give the option of assign the given command or environment name to a package, or to add it to the list of builtin commands. Return a newline-terminated message saying what happened. If ALLOW-EDIT-HERE is non-nil, offer option to edit buffer at current position, and `throw' the symbol `edit-here' with the current point."
  (unless prompt (setq prompt ""))
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  ;; Prompt to add new builtin or add to a package
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
                            "(f) mark it temporarily as a file-local command\n"
                            (when allow-edit-here "(e) edit the buffer here\n")
                            "(↵) do nothing"))))
    (cond
     ((and allow-edit-here (eq decision ?e))
      (throw 'edit-here (point)))
     ((eq decision ?p)
      (choose-and-add-package-or-class c-or-e package-file nil))
    ((eq decision ?D)
     (choose-and-add-package-or-class c-or-e package-file t))
    ((eq decision ?d)
     (choose-and-add-package-or-class c-or-e package-file t
                                      (concat "\\documentclass{" (car (get-document-class)) "}")))
    ((eq decision ?f)
     (let ((fname (local-cmds-filename)))
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

(defun scan-for-other-defined-cmds ()
  "Return a list of all commands defined in the current buffer using commands in `cmdlist-cmd-defining-cmds'."
  ;; This is adapted from scan-for-newcmds. Perhaps something should be factored out?
  (let* ((res)
         (cmds (mapcar (lambda (x) (if (stringp x) x (car x))) cmdlist-cmd-defining-cmds))
         (regx (concat "\\\\\\(" (mapconcat 'regexp-quote cmds "\\|") "\\)[^a-z]")))
    (save-everything
      (goto-char (point-min))
      (while (re-search-forward regx nil t)
        (let ((match (match-string-no-properties 1))
              (defined-cmd
                (progn
                  ;; Take back the "look-ahead" in the regex
                  (backward-char)
                  ;; Remove any surrounding brackets and backslashes
                  (car (split-string (shloop-latex-arg) nil nil "\\\\?[{}]*")))))
          (if (member match cmdlist-cmd-defining-cmds)
              (push defined-cmd res)
            (let ((generated-cmds
                   (funcall (nth 1 (car (member-if (lambda (x)
                                                     (and (listp x)
                                                          (string= (car x) match)))
                                                   cmdlist-cmd-defining-cmds)))
                            defined-cmd)))
              (dolist (c generated-cmds) (push c res)))))))
    (reverse res)))

(defun cmdlist-buffer-provided-cmds (&optional package-file builtin-file)
  "Get all commands which are defined or provided for in this buffer."
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (append (read-lines builtin-file)
          ;; TODO: This is wasteful. These should be combined into a single command.
          (mapcar 'newcmd-name (scan-for-newcmds))
          (mapcar 'newcmd-name (scan-for-newthms))
          (scan-for-other-defined-cmds)
          (scan-for-defined-envs)
          (when (file-exists-p package-file)
            (scan-package-file (append (scan-for-packages) (get-document-class)) t package-file))
          (read-lines (local-cmds-filename))))

(defun cmdlist-package-update-latex-buffer (&optional heading package-file builtin-file)
  "Add to current buffer all ``\\usepackage's defined in PACKAGE-FILE (default is `cmdlist-package-file') under HEADING (default is `cmdlist-package-heading') which provide commands or environments present in the current file and not built-in (according to BUITLIN-FILE, default `cmdlist-builtin-file') or already defined. Prompt to act on any unmatched commands or enviornments."
  (interactive)
  ;; This is adapted from `cmdlist-update-latex-buffer'. Perhaps something should be factored out?
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (let ((cmds-and-envs (append (scan-for-latex-cmds) (scan-for-latex-envs)))
        (exceptions (cmdlist-buffer-provided-cmds package-file builtin-file))
        (curex "")
        (lastmessage ""))
    ;; Warning! (sort) is destructive! (see above warning)
    (setq cmds-and-envs (remove-duplicates (seq-sort 'string< cmds-and-envs)))
    (setq exceptions (remove-duplicates (seq-sort 'string< exceptions)))
    ;; This catches the `edit-here' possibly thrown by `act-on-orphaned-command' and goes to the point thrown by that `throw'
    (let ((edit-point
           (catch 'edit-here
             (dolist (c-or-e cmds-and-envs)
               ;; Check if c-or-e is an exception
               (unless (progn
                         (while (and (string< curex c-or-e)
                                     exceptions)
                           (setq curex (pop exceptions)))
                         (string= curex c-or-e))
                 ;; Find a matching package
                 (let ((pkgmatch (singleton-or-prompt
                                  (match-in-package-file c-or-e t package-file)
                                  (concat "\n Multiple packages provide `"
                                          c-or-e "'. Choose one: "))))
                   ;; If we found one, add it, and refresh exceptions.
                   (if pkgmatch
                       (progn
                         (stick-at-top heading pkgmatch)
                         (setq exceptions (cmdlist-buffer-provided-cmds package-file builtin-file))
                         ;; Warning! (sort) is destructive! (see above warning)
                         (setq exceptions (remove-duplicates (seq-sort 'string< exceptions)))
                         (setq curex ""))
                     ;; Otherwise, act on it
                     (save-everything
                       ;; Display the command in question in the buffer
                       (goto-char (point-min))
                       (re-search-forward (concat "\\\\\\(begin{\\)?{?"
                                                  (regexp-quote c-or-e)
                                                  "\\($\\|[^a-zA-Z]\\)"))
                       (setq lastmessage
                             (act-on-orphaned-command
                              c-or-e lastmessage heading package-file builtin-file t))
                       ;; This is so catch only returns non-nil in the case of a throw
                       nil))))))))
      (when edit-point
        (push-mark)
        (when (member 'evil features)
          (evil-set-jump))
        (goto-char edit-point)))))

(defun cmdlist-package-handle-command (&optional c-or-e heading package-file builtin-file)
  "Like `cmdlist-package-update-latex-buffer', but only do it for given command. If C-OR-E is nil, prompt for command, defaulting to command at point."
  ;; Some of this should probably be factored out too.
  (interactive)
  (unless c-or-e
    (setq c-or-e (read-from-minibuffer "Name of command or environment to handle? \\" (latex-cmd-under-point)))
  (unless heading (setq heading cmdlist-package-heading))
  (unless package-file (setq package-file cmdlist-package-file))
  (unless builtin-file (setq builtin-file cmdlist-builtin-file))
  (let ((exceptions (cmdlist-buffer-provided-cmds package-file builtin-file)))
    (if (member c-or-e exceptions)
        ;; Nothing to do
        (message "%s" (concat "`" c-or-e "' already defined or provided"))
      ;; Find a matching package
      (let ((pkgmatch (singleton-or-prompt
                       (match-in-package-file c-or-e t package-file)
                       (concat "\n Multiple packages provide `" c-or-e "'. Choose one: "))))
        ;; If we found one, add it
        (if pkgmatch
            (stick-at-top heading pkgmatch)
          (message "%s" (act-on-orphaned-command
                         c-or-e nil heading package-file builtin-file nil))))))))

(provide 'cmdlist)
