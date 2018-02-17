# emacs-moin-mode
A major mode for Emacs to support editing [MoinMoin](https://moinmo.in/) wiki pages. The main design premise was to create a similar editing experience as the Emacs Org mode does.

This major mode is meant to be used with MoinMoin 1.9.x markup. It was tested with Emacs 24.4, 24.5 and 25.3 under Windows 7, Windows 10 and Ubuntu 16.04.

I recommend to use [Yasnippet](https://github.com/joaotavora/yasnippet) snippets and [It's all text](https://addons.mozilla.org/en-US/firefox/addon/its-all-text/) for firefox users to further enhance the editing experience of MoinMoin wiki pages.

## Install and Enable 

TODO

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

**Navigation between headings:**
  * TODO

**Changing the document outline:**
  * TODO

**Outline Cycle:** As you might be used to in Org mode, pressing `TAB` if point is on a heading cycles the content belonging to this heading through the following states:
  * 

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
