# emacs-moin-mode
A major mode for Emacs to support editing [MoinMoin](https://moinmo.in/) wiki pages. The main design premise was to create a similar editing experience as the Emacs Org mode does.

This major mode is meant to be used with MoinMoin 1.9.x markup. It was tested with Emacs 24.4, 24.5 and 25.3 under Windows 7, Windows 10 and Ubuntu 16.04.

I recommend to use [Yasnippet](https://github.com/joaotavora/yasnippet) snippets and [It's all text](https://addons.mozilla.org/en-US/firefox/addon/its-all-text/) for firefox users to further enhance the editing experience of MoinMoin wiki pages.

## Install and Enable 

TODO

## Features

Here is what the Emacs Moin mode can do for you:
  * [Syntax highlighting](#Syntax Highlighting)
  * [Formatting support](#Formatting)
  * [Support for headings and outline editing](#Headings)
  * [Support for list editing](#Lists)
  * [Support for table editing](#Tables)

### Syntax Highlighting

The Emacs Moin mode highlights the usual MoinMoin wiki markup constructs such as headings, tables, lists, links, text formatting etc. The faces used are mostly the same as for the Org mode. Of course you can customize all the faces. The face definitions start with "moin-face-". All of them are defined in [moin-faces.el](moin-faces.el).

Here is an example screenshot:

TODO Screenshot

You can see all syntax highlighting faces in action in  [syntaxHighlightingTestBuffer.txt](syntaxHighlightingTestBuffer.txt).

### Formatting

The Emacs Moin mode allows you to set the common formattings by using the following key-bindings

| **Key** | **Function** | **Description** |
| `C-c C-f C-b` | moin-command-format-bold | Insert MoinMoin bold formatting (six ') at point, or, if mark is active, three ' surrounding the highlighted text |
 

### Headings
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
