# Intro

**`cmdlist.el`** is an emacs package for automatically adding `\newcommand` (and `\newtheorem` and `\usepackage`) statements to your LaTeX files based on which commands you use in the document. This is done in two ways:

1. The `\newcommand` and `\newtheorem` statements are pulled from a global file (by default `~/.latex-commands.sty` and `~/.latex-theorems.sty`). Hence, this is an alternative to simply including (i.e., calling `\usepackage` on) a global `.sty` file; the latter has the disadvantage that you cannot safely make changes to your `.sty` file without possibly breaking older documents.
2. The `\usepackage` statements are updated according to another file (by default `~/.latex-packages.sty`) containing a list of which commands are provided by each package (and each documentclass). An example such file (`~/latex-packages.sty.example`) is provided containing many of the most common commands. (It should be possible to generate such a file automatically, but I haven't figured out a good way to do so.)

These two features (let us call them the _command feature_ and the _package feature_) can be used independently.

There is also a third main feature (let us call it the _builtin feature_) (which at present is used automatically with the package feature, but could be easily decoupled if anyone wanted that):

3. You maintain yet another file (by default `~/.latex-builtins`) containing a list of the built-in commands provided by TeX/LaTeX (again, an example `latex-builtings.example` is provided with a list of some of the most common ones). When using this feature, your file is scanned for any command which is not defined in the current file (and therefore wasn't pulled in from your `~/.latex-commands.sty` file) and which is not listed as built-in or provided by any package or documentclass, and you are asked what you would like to do about it (e.g., mark it as built-in or belonging to a package, or just editing it in case it was a typo).

   Thus, when using this feature, "every command is accounted for".

There are also various convenience functions for adding commands to the `~/.latex-commands.sty` file and related actions.

# Usage
## Installation
Add:
```
(require 'cmdlist "/path/to/cmdlist.el")
```
or, if `/path/to/` is in your `load-path`:
```
(require 'cmdlist)
```
to your init file.

You must also create the files `~/.latex-commands.sty`, `~/.latex-theorems.sty`, `~/.latex-packages.sty`, and `~/.latex-builtins` (or the values of the variables `cmdlist-files`, `cmdlist-theorem-file`, `cmdlist-package-file`, and `cmdlist-builtin-file`, respectively).

You would also be wise to copy `latex-packages.sty.example` and `latex-builtins.example` to `~/.latex-packages.sty`, and `~/.latex-builtins` (or the values of the corresponding variables), respectively. You may also want to look at `latex-commands.sty.example` and `latex-theorems.sty.example`.

## The simplest way to use all the main features
Run the command `cmdlist-conditional-update-buffer` whenever you want to update your commands and packages. For this to work, your LaTeX file's preamble should contain (some subset of) the following.
```
% Packages

% Commands

% Theorems
\theoremstyle{definition}

\theoremstyle{remark}

\theoremstyle{plain}
```
(The strings `"% Packages"` and `"% Commands"` and `"% Theorems"` can be customized with the variables `cmdlist-package-heading`, `cmdlist-heading`, and `cmdlist-theorem-heading`.)

There is a convenience function called `cmdlist-update-save-and-compile` which saves the buffer, does `cmdlist-conditional-update-buffer`, and then compiles your file. I recommend binding it to something convenient and getting into the habit of using it whenever you would normally save and/or compile.

## Other useful functions
### Only using one of the command feature or the package feature:

The functions which are called by `cmdlist-conditional-update-buffer`, and which you can call separately if you like, are:
- `cmdlist-update-latex-buffer`
- `cmdlist-newthm-update-latex-buffer`
- `cmdlist-package-update-latex-buffer`

### Checking for extraneous commands:
- `cmdlist-show-unused-newcmds` shows all the commands and theorems defined in the present file which are not being used.
- `cmdlist-delete-unused-newcmds` does the same, and also offers to delete them.

There should be a similar function for unused packages, but there isn't yet.

### Adding new commands

`generate-and-add-cmd` queries for a new command definition and adds it to the present file or to the global command file (based on `cmdlist-add-to-file-default` and whether there's a prefix argument). If the definition involves arguments (like `#1`), the generated command will have the correct number of arguments. and you will also be asked if one of the arguments should be an optional argument, and if so, what the default optional argument should be.

There are also variations which add commands of the form `\newcommand\name{\somefont{X}}`. These are:
- `cmdlist-generate-mathrm`
- `cmdlist-generate-mathbf`
- `cmdlist-generate-mathbb`
- `cmdlist-generate-mathcal`
- `cmdlist-generate-mathfrak`
- `cmdlist-generate-operatorname`

If you are interested in writing your own functions along these lines, you may want to look at the definitions of `cmdlist-generate-and-add-fontletter`, `cmdlist-add-newcmd`, and `cmdlist-stick-at-top`.

`cmdlist-add-cmd-to-file` adds the command definition under point to the global command file.

`cmdlist-generate-package` asks for a package name and sticks it under the heading `% Packages`.

### Viewing the global command file

`cmdlist-open-cmdlist-file` opens the global command file in the other window and asks you (with completion) for a command to look at (this uses [swiper](https://github.com/abo-abo/swiper) if you have it installed).

### Testing a command in a minimal environment.

`cmdlist-test-command-in-minimal-file` prompts for a command (default is the one under point) and creates a minimal latex file with that command inserted. This is useful if you want to try to figure out which package is providing a certain command.

This function is also called when `cmdlist-package-update-latex-buffer` finds an unrecognized command and asks you what to do, and you select "test it in a minimal LaTeX file".

## Adding other command-defining commands

As explained above, `cmdlist-package-update-latex-buffer` implements an "every command is accounted for" rule, and will ask you about any command which isn't defined in a `\(re)newcommmand` or `\newtheorem` in the present file, or provided by a package. However, there are other commands which define commands such as `\let`, `\def`, and `\newif`.

You can customize which such commands are recognized with the variable `cmdlist-cmd-defining-cmds`.

## Syntax of the files `.latex-theorems.sty` and `latex-packages.sty`

Most of the time, you do not need to edit any of the files (`.latex-commands.sty`, etc.) by hand -- they are updated automatically by various functions. But sometimes, you might need to go in and edit them by hand (and in fact, there are presently no functions for modifying `.latex-theorems.sty`).

The files `.latex-commands.sty` and `.latex-builtins` have a fairly self-explanatory syntax (look at the provided `.example` files!). The former simply has one `\(re)newcommand` statement per line (possibly with a comment, which is simply included when the command is imported), and the latter simply has one command name per line.

### `.latex-theorems.sty`
This file is similar to `.latex-commands.sty`, except that, if a line has a comment, it should just be a single word (and should probably be `definition` or `remark`). Here is an example line:
```
\newtheorem{defn}{Definition}%definition
```
When this line is imported, the comment is removed, and it is placed under the line `\theoremstyle{X}` in your LaTeX file, where `X` is the word appearing in the comment. If there is no comment, it is placed under `\theoremstyle{plain}`.

Another thing about importing `\newtheorem` statements: when this is done, if the variable `cmdlist-default-shared-counter` is non-nil (the default value is `"defn"`), its value is added as an optional argument to `\newtheorem` after the first argument, i.e., `\newtheorem{X}[here]{Y}`. Otherwise, if `cmdlist-default-parent-counter` is non-nil, its value is added as an optional argument after the second argument, i.e., `\newtheorem{X}{Y}[here]`.

### `.latex-packages.sty`
The file `latex-packages.sty` is a bit more complicated. There are four types of lines. Here are examples of all four:
```
\usepackage{pgfplots}%axis,empty
\usepackage{tikz}%path%xcolor
\documentclass{article}%part
\documentclass{amsart}%part,qed,qedsymbol%amsthm,amsmath
```
So we see that a line can either be a `\usepackage` statement or a `\documentclass` statement. There is then always a comment containing a comma-separated list of names of commands. These are commands provided by that package or documentclass.

Moreover, the comment may contain a further `%` sign, followed by a comma-separated list of package names. These are packages which are provided by that package or document class. When `cmdlist-package-update-latex-buffer` is checking whether a given command is already provided by a currently imported package or the current documentclass, it recursively also checks those packages provided by the currently imported packages, and so on.

## Other variables for customization
We summarize the variables available for customization.

* `cmdlist-files`: see above (note that this is a list of filenames, all of which are used draw commands from; the first entry in the list is the file which new commands are added to)

  Default: `"~/.latex-commands.sty"`

* `cmdlist-theorem-file`: see above

  Default: `"~/.latex-theorems.sty"`

* `cmdlist-package-file`: see above

  Default: `"~/.latex-packages.sty"`

* `cmdlist-builtin-file`: see above

  Default: `"~/.latex-builtins"`

* `cmdlist-heading`: see above

  Default: `"% Commands"`

* `cmdlist-package-heading` : see above

  Default: `"% Packages"`

* `cmdlist-theorem-heading`: the heading which `cmdlist-conditional-update-buffer` looks for in deciding whether to update theorems.

  Default: `"% Theorems"`

* `cmdlist-visit-after-adding`: whether to visit the global command file after adding a new command to it.

  Default: `nil`.

* `cmdlist-also-add-to-buffer`: If non-nil, also add command to buffer when adding to global command list, unless value is `'ask`, in which case ask first.

  Default: `nil`

* `cmdlist-add-to-file-default`: see above

  Default: `nil`

* `cmdlist-braces-around-cmd-name`: whether to include braces around the names of newly generated commands (as in `\newcommand{\foo}`)

  Default: `nil`

* `cmdlist-default-shared-counter`: see above

  Default: `"defn"`

* `cmdlist-default-parent-counter` see above

  Default: nil

* `cmdlist-ignore-at-symbol`: If non-nil, `cmdlist-package-update-latex-buffer` will ignore commands containing `@`. (Note that, in any case, `@` is always treated as part of a command name, which will be incorrect if not within `\makeatletter` region.)

  Default: `t`

* `cmdlist-newcommands-to-ignores`: List of commands that should not be taken from `cmdlist-files` by `cmdlist-update-latex-buffer`. This can be used, for example, to prevent a \"renewcommand\" of a builtin command from getting inserted.
* `cmdlist-test-minimal-file`: Path of file created by `cmdlist-test-command-in-minimal-file` (which is called during `cmdlist-package-update-latex-buffer` when you come across an unrecognized command and select \"test it in a minimal LaTeX file\").

  Default: `"/tmp/minimal.tex"`


## Example keybindings

Here are the keybindings I use (with `evil-mode`):
```
(evil-define-key 'normal LaTeX-mode-map
  (kbd "SPC d c") 'cmdlist-update-save-and-compile
  (kbd "SPC g u") 'cmdlist-conditional-update-buffer
  (kbd "SPC g c") 'cmdlist-generate-and-add-cmd
  (kbd "SPC g a") 'cmdlist-add-cmd-to-file
  (kbd "SPC g r") 'cmdlist-generate-mathrm
  (kbd "SPC g b") 'cmdlist-generate-mathbf
  (kbd "SPC g B") 'cmdlist-generate-mathbb
  (kbd "SPC g C") 'cmdlist-generate-mathcal
  (kbd "SPC g F") 'cmdlist-generate-mathfrak
  (kbd "SPC g o") 'cmdlist-generate-operatorname
  (kbd "SPC g p") 'cmdlist-generate-package
  (kbd "SPC g z") 'cmdlist-delete-unused-newcmds
  (kbd "SPC g f") 'cmdlist-open-cmdlist-file)
```

(And in fact I also have
```
(evil-define-key '(normal motion) 'global
   (kbd "SPC d c") 'save-buffer)
```
just because I'm so much in the habit now of typing `SPC d c` whenever I want to save.)

# Kaizen
I hope you use and enjoy this package! There are presumably a bunch of bugs. Please tell me about them if you find them! Also, please contribute any bug fixes or other improvements you make.

And especially: if you beef up your `.latex-packages.sty` and `.latex-builtins` files, please send me your beefed up files so I can incorporate them into the repository.
