# Emacs Moin mode
A major mode for Emacs to support editing [MoinMoin](https://moinmo.in/) wiki pages. The main design premise was to create a similar editing experience as the Emacs Org mode does.

This major mode is meant to be used with MoinMoin 1.9.x markup. It was tested with Emacs 24.4, 24.5 and 25.3 under Windows 7, Windows 10 and Ubuntu 16.04.

I recommend to use [Yasnippet](https://github.com/joaotavora/yasnippet) snippets and [It's all text](https://addons.mozilla.org/en-US/firefox/addon/its-all-text/) for firefox users to further enhance the editing experience of MoinMoin wiki pages.

## Install and Enable 

Easy installation using Melpa is still in progress. For now, download all .el and put them into a suitable folder. Ensure Emacs knows the moin-mode module by adding the following to your init.el or .emacs file:
```elisp
(add-to-list 'load-path "/path/to/moin-mode-dir/")
(require 'moin-mode)
```
Enable moin-mode in a buffer by typing `M-x moin-mode`.

## Features

Here is what the Emacs Moin mode can do for you:
  * [Syntax highlighting](#syntax-highlighting)
  * [Formatting support](#formatting)
  * [Support for headings and outline editing](#headings)
  * [Support for list editing](#lists)
  * [Support for table editing](#lables)

### Syntax Highlighting

The Emacs Moin mode highlights the usual MoinMoin wiki markup constructs such as headings, tables, lists, links, text formatting etc. The faces used are mostly the same as for the Org mode. Of course you can customize all the faces. The face definitions start with "moin-face-". All of them are defined in [moin-faces.el](moin-faces.el).

Here is an example screenshot:

TODO Screenshot

You can see all syntax highlighting faces in action in  [syntaxHighlightingTestBuffer.txt](syntaxHighlightingTestBuffer.txt).

### Formatting

The Emacs Moin mode allows you to set the common formattings by using the following key-bindings

  * `C-c C-f C-b` - `moin-command-format-bold` - Insert MoinMoin bold formatting (six ') at point and places point in the middle, or, if mark is active, three ' surrounding the currently selected text
  * `C-c C-f C-i`, `C-c C-f C-e` - `moin-command-format-italic` - Insert MoinMoin italic formatting (four ') at point and places point in the middle, or, if mark is active, two ' surrounding the currently selected text
  * `C-c C-f C-u` - `moin-command-format-underline` - Insert MoinMoin underline formatting (two _) at point and places point in the middle, or, if mark is active, one _ surrounding the currently selected text

### Headings

The Emacs Moin mode is based on Emacs outline-mode, but extra tweaking was necessary to make it work with MoinMoin's enclosing "=" markups, and it adds an outline cycle feature.

**Navigate between headings:** (inherited from outline-mode, but mapped to slightly different keys)
  * `C-c C-x C-n` - `outline-next-visible-heading` - Move to the next visible heading
  * `C-c C-x C-p` - `outline-previous-visible-heading` - Move to the previous visible heading
  * `C-c C-x C-f` - `outline-forward-same-level` - Move fo the next visible heading on the same level
  * `C-c C-x C-b` - `outline-backward-same-level` - Move fo the previous visible heading on the same level
  * `C-c C-x C-u` - `outline-up-heading` - Move to the next higher level heading

**Outline Cycle:** As you might be used to in Org mode, pressing `TAB` if point is on a heading cycles the content belonging to this heading through the following states:
  * `FOLDED`: Hides the entire subtree and content of the current heading
  * `CHILDREN`: Shows the content of the current heading and all direct child headings of the next lower level below the current heading. The child heading subtrees remain hidden.
  * `SUBTREE`: The entire content and subtree below the current heading is shown entirely, no folding.

**Create new headings:**
  * `M-<return>` - `moin-command-meta-return` - If point is outside of a list and outside of a table, this command inserts a new heading on the next line, splitting any content at point. The new heading has the same level as the previous heading, or level 1, if there is no previous heading.
  * `C-<return>` - `moin-command-insert-heading-respect-content` - This inserts a new heading just before the next heading (or end of document, respectively) on the same level as the previous heading or level 1, if there is no previous heading.

**Change the document's outline:**
  * `M-<left>` - `moin-command-meta-left` - If point is currently on a heading line, it promotes the current heading, leaving its children unchanged, but only if the current heading is not already on level 1. This command does not work with an active mark, which will lead to an error being thrown. It also fixes   errors in the end marker of the heading, if necessary.
  * `M-S-<left>` - `moin-command-meta-shift-left` - If point is currently on a heading line, it promotes the current subtree (i.e. the current heading and all its children), if the current heading is not already on level 1. This command does not work with an active region, which will lead to an error being thrown. It also fixes errors in the end marker of the heading, if necessary.
  * `M-<right>` - `moin-command-meta-right` - If point is currently on a heading line, it demotes the current heading, leaving its children unchanged, but only if the current heading is not already on level 5. This command does not work with an active mark, which will lead to an error being thrown. It also fixes errors in the end marker of the heading, if necessary.
  * `M-S-<right>` - `moin-command-meta-shift-right` - If point is currently on a heading line, it demotes the current subtree (i.e. the current heading and all its children), if the current heading is not already on level 5. This command does not work with an active region, which will lead to an error being thrown. It also fixes errors in the end marker of the heading, if necessary.
  * `M-<up>`, `M-S-<up>` - `moin-command-meta-up`, `moin-command-meta-shift-up` - If point is currently on a heading line, it basically does the same thing as `outline-move-subtree-up`: It moves the subtree of the current heading to the point before the previous heading of the same level, if any. If there is no such heading, it throws an error.
  * `M-<down>`, `M-S-<down>` - `moin-command-meta-down`, `moin-command-meta-shift-down` - If point is currently on a heading line, it basically does the same thing as `outline-move-subtree-down`: It moves the subtree of the current heading to the point after the subtree of the next heading of the same level, if any. If there is no such heading, it throws an error.

### Lists
### Tables

## Known Issues
## Planned Features

The following features sound like worthy additions:
  * Support for colspan and rowspan tables
  * Index of local headings to use for hyperlinking
  * Tackling some other of the known issues

## Bug Reports

Please create a bug in the [GitHub bug tracker](https://github.com/jebbard/emacs-moin-mode/issues).
