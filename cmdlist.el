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

;; TODO Make this also handle \\newtheorem
;; TODO Maybe some automatic package handling too...
;; TODO Adapt sort-newcmds for \\usepackage

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(defvar cmdlist-files '("~/.latex-commands.sty")
  "List of files containing `\\newcommand' entires to be used by `cmdlist.el'. The first entry is also used to store new commands.")

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

;;;;;;;;;;;;;;;;;;;;;;
;; Parsing commands ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun shloop-latex-arg ()
    "Move past the latex argument (bracket expression, command name, or single character) starting under point, and return it"
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
    (save-mark-and-excursion
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
    (save-mark-and-excursion
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
    (save-mark-and-excursion
      (if (re-search-backward regex nil t)
          (progn (setq place (match-beginning 0))
                 (forward-char))
        (goto-char (point-min)))
      (when (and (re-search-forward regex nil t)
                 (<= (match-beginning 0) start))
        (setq place (match-beginning 0))))
    (when place
      (goto-char place))))

(defun surrounding-newcmd ()
  "If point is not inside of a latex `\\newcommand', return nil. Otherwise, return the text of the whole command."
  (let ((start (point))
        (beg))
    (save-mark-and-excursion
      (when (re-search-backward-incl "\\\\r?e?newcommand")
        (setq beg (point))
        (search-forward-regexp "\\\\r?e?newcommand")
        (when (eq (char-after) ?\{)
          (forward-brexp))
        (forward-brexp)
        (when (>= (point) start)
          (end-of-line)
          (buffer-substring-no-properties beg (point)))))))

(defun scan-for-latex-cmds (&optional ignore-newcmds)
  "Return all names of latex commands in current buffer."
  (let (cmds last-newcmd-pos)
    (save-restriction
      (save-mark-and-excursion
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
  "Return the name of the given \\newcommmand."
  (save-mark-and-excursion
    (with-temp-buffer
      (insert cmd)
      (goto-char (point-min))
      (re-search-forward "\\\\r?e?newcommand{?")
      (substring (shloop-latex-arg) 1))))

(defun scan-for-newcmds ()
  "Return a list of all \\newcommmands in the current buffer"
  (let ((res))
    (save-restriction
      (save-mark-and-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\\\r?e?newcommand" nil t)
          (push (surrounding-newcmd) res)
          (forward-brexp)))
    (reverse res))))

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
    (save-mark-and-excursion
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
  (save-excursion
    (save-restriction
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
                   (lambda () (re-search-forward "\\\\r?e?newcommand{?") nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding commands in a list of \newcommands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cmdlist-matches (cmdlist name)
  "Return all elements of CMDLIST which are `\\newcommand's defining the command NAME."
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
    (save-restriction
      (if (buffer-narrowed-p) (widen))
      (save-excursion
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
          (funcall sortfun nil starting-point (point)))))))

(defun add-to-cmdlist-file (newcmd &optional file)
  "Add NEWCMD to FILE (default: the first entry in `cmdlist-files') and then sort the lines.

If the command being defined in NEWCMD is already in FILE, ask for confirmation."
  (unless file (setq file (car cmdlist-files)))
  (if (and (file-readable-p file) (file-writable-p file))
      (let ((cmdlist (scan-file-for-newcmds file)))
        (if (cmdlist-already-p cmdlist newcmd)
            (save-mark-and-excursion
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
  "Delete (seemingly) unused `\\newcommand's in this buffer, after prompting."
  (interactive)
  (let ((unuseds (get-unused-newcmds)))
    (if unuseds
        (when (y-or-n-p (format "Seemingly unused commands:\n%s\nDelete? " (list-of-things unuseds)))
          (dolist (x unuseds)
            (save-mark-and-excursion
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
    (setq cmd (completing-read "Goto command: "
                               (mapcar 'newcmd-name (scan-for-newcmds))
                               nil nil nil nil cmd))
    (when cmd
      (let ((cmd-pos))
        (save-mark-and-excursion
          (goto-char (point-min))
          (when (re-search-forward (format "\\\\r?e?newcommand{?\\\\%s[}{\\[]" cmd) nil t)
            (setq cmd-pos (point))))
        (when cmd-pos
          (goto-char cmd-pos))))))
