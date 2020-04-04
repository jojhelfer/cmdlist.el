# Intro

**`cmdlist.el`** is an attempt to solve the problem of where to keep your LaTeX commands. Keeping a global style file is convenient since you only have to add a command once, but it makes it dangerous to change it since it can break older documents.

`cmdlist.el` draws commands from a global file containing a list of commands and updates the preamble with whichever of those commands are being used in the current file. It also provides some convenience functions for maintaining the command file.

# Usage
## Installation
There is probably a better way to do this, but for now, do:
```
(load-file "/path/to/cmdlist.el")
```
## Main commands
Here are the commands defined in `cmdlist.el`:

* Updating the preamble

   `cmdlist-update-latex-buffer` searches `~/.latex-commands.sty` (can be customized with `cmdlist-files`) for the definition of latex commands being used in the current file and adds any missing ones under `% Commands` (can be customized with `cmdlist-heading`) and then sorts the commands alphabetically.

* Checking for extraneous commands

   `show-unused-newcmds` shows all the commands defined in the present file which are not being used.

   `delete-unused-newcmds` does the same, and also offers to delete them.

* Adding new commands

   `generate-and-add-cmd` queries for a new command definition and adds it to the present file or to the global command file (based on `cmdlist-add-to-file-default` and whether there's a prefix argument).

   There are also variations which add commands of the form `\newcommand\name{\somefont{X}}`. These are:
   - `generate-mathrm`
   - `generate-mathbf`
   - `generate-mathbb`

   `add-cmd-to-file` adds the command definition under point to the global command file.

   Finally, `generate-package` asks for a package name and sticks it under the heading `% Packages`. Hopefully a future version will be able to update packages automatically based on what commands are present.

* Viewing the global command file

   `open-cmdlist-file` opens the global command file in the other window.

## Rolling your own

If you want to extend or experiment with the code, I draw your attention to the following (non-interactive) functions:

* `generate-and-add-fontletter`: used in the definition of `generate-mathrm`, etc.
* `stick-at-top`: for `generate-package`-like functionality
* `add-newcmd`: used in `generate-and-add-cmd`

## Variables for customization
We summarize the variables available for customization.

* `cmdlist-files`: a list of filenames from which to draw commands. The first entry in the list is the file which new commands are added to.  
   Default: `~/.latex-commands.sty`
* `cmdlist-heading`: the heading under which new commands are added in a latex file.  
   Default: `% Commands`
* `cmdlist-visit-after-adding`: whether to visit the global command file after adding a new command to it.  
   Default: `t`.
* `cmdlist-add-to-file-default`: whether `generate-and-add-cmd` and friends by default (i.e., without a prefix argument) add to the global command file rather than the current buffer.  
   Default: `nil`
* `cmdlist-braces-around-cmd-name`: whether to include braces around the names of newly generated commands (as in `\newcommand{\foo}`)  
   Default: `nil`

## Example keybindings

Here are the keybindings I use (with `evil-mode`):
```
(evil-define-key 'normal LaTeX-mode-map
  (kbd "SPC g u") 'cmdlist-update-latex-buffer
  (kbd "SPC g c") 'generate-and-add-cmd
  (kbd "SPC g a") 'add-cmd-to-file
  (kbd "SPC g r") 'generate-mathrm
  (kbd "SPC g b") 'generate-mathbf
  (kbd "SPC g B") 'generate-mathbb
  (kbd "SPC g p") 'generate-package
  (kbd "SPC g z") 'delete-unused-newcmds
  (kbd "SPC g f") 'open-cmdlist-file)
```

I also recommend binding
```
(defun update-macros-save-and-compile ()
  "Update macros if the cmdlist heading is present, save buffer, and then compile Latex."
  (interactive)
  (let ((has-heading nil))
    (save-mark-and-excursion
      (goto-char (point-min))
      (setq has-heading (search-forward cmdlist-heading nil t)))
    (when has-heading
      (cmdlist-update-latex-buffer)))
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file nil))
```
to something.
