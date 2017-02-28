;;; moin-mode.el --- a major mode to edit MoinMoin 1.9.x wiki pages

;; Copyright (C) 2017 Jens Ebert

;; Author: Jens Ebert <jensebert@gmx.net>
;; Maintainer: Jens Ebert <jensebert@gmx.net>
;; Created: 20 Jan 2017
;; Keywords: wiki editing
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Overview:
;; ---------
;; This is moin-mode which targets to provide a rich editing feeling for
;; MoinMoin 1.9.x wiki pages. See <https://moinmo.in/> for more information
;; regarding MoinMoin. The major editing facilities and syntax highlighting
;; is designed to be quite similar to org mode 8.x.
;; It is possible yet not guaranteed that this mode will work as expected
;; also with other versions of MoinMoin markup, namely Moin 1.8 and below.
;;
;; This major mode tries to make the rather outdated moinmoin-mode.el as found
;; on the following page obsolete:
;; <https://moinmo.in/EmacsForMoinMoin>
;;
;; This is an entirely new major mode for MoinMoin, created from the ground up
;; but surely inspired by the existing work.
;;
;; Installation:
;; -------------
;; * Ensure the file `moin-mode.el' is located in Emacs load
;; path, if this is not the case, use `add-to-list' in your Emacs
;; initialization file, e.g. to .emacs:
;;   (add-to-list 'load-path /my/dir)
;; * Add the following line to your Emacs initialization file:
;;   (require 'moin-mode)
;;
;; User documentation & configuration:
;; -----------------------------------
;; For more details on how to use and configure `moin-mode', see
;; the documentation of the `moin-mode' major mode by typing
;;   M-x describe-function [RET] moin-mode
;; in Emacs. Before this, ensure `moin-mode' is installed as
;; described above.
;;
;; Requirements:
;; -------------
;;
;; * Requires minimum Emacs version 24.0
;; * Optional requirement: If you want to use outline-cycle (folding headings
;;   like in org mode), you have to install outshine, minimum version 2.0
;;
;; Tests:
;; ------
;; This module has been tested with all combinations of:
;; * OS: Windows 10, Ubuntu ??
;; * Emacs: 24.5
;; * MoinMoin: 1.9.??
;; * Outshine: 2.0 (specifically 20161024.2158 from ELPA)
;;
;; Improvements:
;; -------------
;; * TBD
::
;; If you have any further suggestions for improvements or enhancements,
;; please mail to <jensebert@gmx.net>

;;; Change Log:
;;
;; v0.1    2017-01-20  Jens Ebert            <jensebert@gmx.net>
;; - initial draft

;;; Code:
;; ==================================================
;; Required mandatory packages

;; Ensure that outshine is installed
(require 'color)

;; ==================================================
;; Group definitions
(defgroup moin nil
  "Major mode for MoinMoin 1.9.x wiki pages"
  :prefix "moin-")

(defgroup moin-faces nil
  "Faces used for font-lock in moin-mode"
  :prefix "moin-face-"
  :group 'moin)

;; ==================================================
;; Customization options

(defcustom moin-highlight-colored-env-p t
  "Set to t to enable the background color syntax highlighting
of !#wiki environments, set to nil otherwise. Default is t"
  :group 'moin)

(defcustom moin-highlight-colored-table-p t
  "Set to t to enable the background color syntax highlighting
of tables, set to nil otherwise. Default is t"
  :group 'moin)


;; ==================================================
;; Buffer local variables

;; NONE yet

;; ==================================================
;; Face definitions

; ------
; Highlighting Formatted Text
; -------
(defface moin-face-bold '((t (:weight bold)))
  "Face name to use for bold text in moinmoin"
  :group 'moin-faces)
(defface moin-face-italic '((t (:slant italic)))
  "Face name to use for italic text in moinmoin"
  :group 'moin-faces)
(defface moin-face-underline '((t (:underline t)))
  "Face name to use for underlined text in moinmoin"
  :group 'moin-faces)
(defface moin-face-stroke '((t (:strike-through t)))
  "Face name to use for stroked text in moinmoin"
  :group 'moin-faces)
(defface moin-face-subscript '((t (:height 0.8)))
  "Face name to use for subscripts in moinmoin"
  :group 'moin-faces)
(defface moin-face-superscript '((t (:height 0.8)))
  "Face name to use for superscripts in moinmoin"
  :group 'moin-faces)
(defface moin-face-monospace '((t (:family "Courier")))
  "Face name to use for typewriter text in moinmoin"
  :group 'moin-faces)
(defface moin-face-larger '((t (:height 1.1)))
  "Face name to use for larger text in moinmoin"
  :group 'moin-faces)
(defface moin-face-smaller '((t (:height 0.9)))
  "Face name to use for smaller text in moinmoin"
  :group 'moin-faces)

; ------
; Highlighting Tables
; -------
(defface moin-face-table-separator
  '((((class color) (background light)) (:foreground "Blue1"))
    (((class color) (background dark)) (:foreground "LightSkyBlue")))
  "Face name to use for separation of columns in tables of moinmoin"
  :group 'moin-faces)
(defface moin-face-table-content
  '((((class color) (background light)) (:foreground "Blue1"))
    (((class color) (background dark)) (:foreground "LightSkyBlue")))
  "Face name to use for separation of columns in tables of moinmoin"
  :group 'moin-faces)
(defface moin-face-table-processing-instruction
  '((((class color) (background light)) (:foreground "DarkGray" :weight bold))
    (((class color) (background dark)) (:foreground "Gray70" :weight bold)))
  "Face name to use for processing instructions in tables of moinmoin"
  :group 'moin-faces)

; ------
; Highlighting Macros
; -------
(defface moin-face-macro-content '((t (:foreground "Dark Blue")))
  "Face name to use for names of macros in moinmoin"
  :group 'moin-faces)
(defface moin-face-macro-braces
  '((((class color) (background light)) (:foreground "Gray70"))
    (((class color) (background dark)) (:foreground "DarkGray")))
  "Face name to use for parameters of macros in moinmoin"
  :group 'moin-faces)

; ------
; Highlighting Embeddings (e.g. attachments)
; -------
(defface moin-face-embedding-content '((t (:foreground "Orange")))
  "Face name to use for names of macros in moinmoin"
  :group 'moin-faces)
(defface moin-face-embedding-braces
  '((((class color) (background light)) (:foreground "Gray70"))
    (((class color) (background dark)) (:foreground "DarkGray")))
  "Face name to use for parameters of macros in moinmoin"
  :group 'moin-faces)

; ------
; Highlighting Headings
; -------
(defface moin-face-h1
  '((((class color) (background light)) (:height 1.4 :foreground "Blue1"))
    (((class color) (background dark)) (:height 1.4 :foreground "LightSkyBlue")))
  "Face name to use for 1-level headings in moinmoin"
  :group 'moin-faces)
(defface moin-face-h2
  '((((class color) (background light)) (:height 1.3 :foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:height 1.3 :foreground "LightGoldenrod")))
  "Face name to use for 2-level headings in moinmoin"
  :group 'moin-faces)
(defface moin-face-h3
  '((((class color) (background light)) (:height 1.2 :foreground "Purple"))
    (((class color) (background dark)) (:height 1.2 :foreground "Cyan1")))
  "Face name to use for 3-level headings in moinmoin"
  :group 'moin-faces)
(defface moin-face-h4
  '((((class color) (background light)) (:height 1.1 :foreground "Firebrick"))
    (((class color) (background dark)) (:height 1.1 :foreground "chocolate1")))
  "Face name to use for 4-level headings in moinmoin"
  :group 'moin-faces)
(defface moin-face-h5
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen")))
  "Face name to use for 5-level headings in moinmoin"
  :group 'moin-faces)

; ------
; Highlighting Environments
; -------

(defconst moin-face-env-light-fg "grey30"
  "The foreground color to use in a code environment in case of light background")
(defconst moin-face-env-dark-fg "white"
  "The foreground color to use in a code environment in case of dark background")

(defface moin-face-env
  '((((class color) (background light)) (:foreground "grey30" :inherit 'moin-face-monospace))
    (((class color) (background dark)) (:foreground "white" :inherit 'moin-face-monospace)))
  "Face name to use for code inside braces in moinmoin"
  :group 'moin-faces)
(defface moin-face-env-braces
  '((((class color) (background light)) (:foreground "Gray70"))
    (((class color) (background dark)) (:foreground "DarkGray")))
  "Face name to use for baces which delimit environments in moinmoin"
  :group 'moin-faces)
(defface moin-face-env-parser '((t (:foreground "plum3" :weight bold)))
  "Face name to use for parser specs"
  :group 'moin-faces)
(defface moin-face-env-usual-bg
  '((((class color) (background light)) (:background "black" :inherit 'moin-face-monospace))
    (((class color) (background dark)) (:background "white" :inherit 'moin-face-monospace)))
  "Face name to use for parser specs"
  :group 'moin-faces)

; ------
; Links
; -------
(defface moin-face-link
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan")))
  "Face name to use for rules in moinmoin"
  :group 'moin-faces)
(defface moin-face-email '((t (:inherit 'moin-face-link :underline t)))
  "Face name to use for emails in moinmoin"
  :group 'moin-faces)
(defface moin-face-link-brackets '((t (:inherit 'moin-face-link :weight bold)))
  "Face name to use for rules in moinmoin"
  :group 'moin-faces)
(defface moin-face-followable-link '((t (:inherit 'moin-face-link :underline t)))
  "Face name to use for rules in moinmoin"
  :group 'moin-faces)
(defface moin-face-wiki-word '((t (:foreground "blue4" :weight bold)))
  "Face name to use for CamelCase links in moinmoin"
  :group 'moin-faces)

; ------
; Highlighting various other elements
; -------
(defface moin-face-rule '((t (:foreground "tomato2" :weight bold)))
  "Face name to use for rules in moinmoin"
  :group 'moin-faces)
(defface moin-face-comment '((t (:foreground "maroon3")))
  "Face name to use for comments in moinmoin"
  :group 'moin-faces)
(defface moin-face-variable '((t (:inherit 'moin-face-monospace)))
  "Face name to use for code inside braces in moinmoin"
  :group 'moin-faces)
(defface moin-face-processing-instruction '((t (:foreground "maroon3")))
  "Face name to use for comments in moinmoin"
  :group 'moin-faces)


;; ==================================================
;; "Private" Functions
(defun moin--create-dynamic-bg-face(bg-color-name)
  "This function dynamically creates a new face symbol for the given 
background color name and sets the background color in the returned face.
It can be used to dynamically change the background color of a piece of 
fontified buffer based on its contents (e.g. background color of a table based
on the specified wiki color). It returns a default background color if the given
color could not be identified (e.g. during typing)."
  (if (color-defined-p bg-color-name)
    (progn
      ; Get the face symbol if already defined before
      (setq face-symbol (intern-soft bg-color-name))

      ; Or create it newly if it wasn't
      (if face-symbol nil
	(progn
	  (setq face-symbol (intern (concat "moin-face-env-" bg-color-name)))))
  
      (face-spec-set face-symbol '((t (:inherit 'moin-face-env-usual-bg))))

      (message "color dist %s" (color-name-to-rgb bg-color-name))
      (setq rgb-bg-color (color-name-to-rgb bg-color-name))
      (setq r (car rgb-bg-color))
      (setq g (car (cdr rgb-bg-color)))
      (setq b (car (cdr (cdr rgb-bg-color))))

      ; Set the foreground color based on the "darkness" of the bg color
      (if (> (+ r g b) 1.5)
         (setq fg-color-name moin-face-env-light-fg)
	  (setq fg-color-name moin-face-env-dark-fg))
      
      (set-face-background face-symbol bg-color-name)
      (set-face-foreground face-symbol fg-color-name)
      ;; Return the newly created face symbol  
      face-symbol)
    ;; If no defined color, return the default background color
    'moin-face-env-usual-bg))

(defun moin--setup-font-lock ()
  "Installs all font lock patterns for syntax highlighting in their required order"
  (setq font-lock-multiline t)
  (make-local-variable 'font-lock-extra-managed-props)
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords nil `(
    ; ------
    ; Highlighting Formatted Text
    ; -------
    ;; Bold
    ("\\('''.*?'''\\)"
     (1 'moin-face-bold prepend))
    ;; Italic
    ("[^']+\\(''[^']*?''\\)[^']+"
     (1 'moin-face-italic prepend))
    ;; Underline
    ("\\(__.*?__\\)"
     (1 'moin-face-underline prepend))
    ;; Strikethrough
    ("\\(--(.*?)--\\)"
     (1 'moin-face-stroke prepend))
    ;; Subscript
    ("\\(,,.*?,,\\)"
     (1 (list 'face 'moin-face-subscript 'display '(raise -0.3)) prepend))
    ;; Superscript
    ("\\(\\^.*?\\^\\)"
     (1 (list 'face 'moin-face-superscript 'display '(raise 0.3)) prepend))
    ;; Monospace
    ("\\(`.*?`\\)"
     (1 'moin-face-monospace prepend))
    ;; Larger
    ("\\(~\\+.*?\\+~\\)"
     (1 'moin-face-larger prepend))
    ;; Smaller
    ("\\(~-.*?-~\\)"
     (1 'moin-face-smaller prepend))

    ; ------
    ; Highlighting Tables
    ; -------
    ;; Table content until next separator
    ("\\(.*?\\)\\(||\\)"
     (1 'moin-face-table-content prepend)
     (2 'moin-face-table-separator))
    ;; Table processing instruction
    ("||\\(<.*?>\\)"
     (1 'moin-face-table-processing-instruction prepend))
    ;; Table with color spec
    ("^||<rowbgcolor=\"\\(.*?\\)\">.*$"
     (0 (when moin-highlight-colored-table-p (moin--create-dynamic-bg-face (match-string 1))) prepend))

    ; ------
    ; Highlighting Macros
    ; -------
    ;; Macro
    ("\\(<<\\)\\(.*?\\)\\(>>\\)"
     (1 'moin-face-macro-braces)
     (2 'moin-face-macro-content prepend)
     (3 'moin-face-macro-braces))

    ; ------
    ; Highlighting Embeddings (e.g. Attachment)
    ; -------
    ;; Embedding
    ("\\([^{]{{\\)\\([^{].*?[^}]\\)\\(}}[^}]\\)"
     (1 'moin-face-embedding-braces)
     (2 'moin-face-embedding-content prepend)
     (3 'moin-face-embedding-braces))
    
    ; ------
    ; Highlighting Headings
    ; -------
    ("^\\(= .* =\\)$"
     (1 'moin-face-h1 t))
    ("^\\(== .* ==\\)$"
     (1 'moin-face-h2 t))
    ("^\\(=== .* ===\\)$"
     (1 'moin-face-h3 t))
    ("^\\(==== .* ====\\)$"
     (1 'moin-face-h4 t))
    ("^\\(===== .* =====\\)$"
     (1 'moin-face-h5 t))
    
    ; ------
    ; Highlighting Environments
    ; -------
    ;; One line & multiline code environments with color spec
    ("\\({\\{3,4\\}\\)\\(#!wiki \\([a-z]*?\\)/[a-z]+?$\\)\\([[:ascii:][:nonascii:]]*?\\)\\(}\\{3,4\\}\\)"
     (1 'moin-face-env-braces prepend)
     (2 'moin-face-env-parser prepend)
     (3 'moin-face-env-parser prepend)
     (4 'moin-face-env t)
     (5 'moin-face-env-braces prepend)
     (0 (when moin-highlight-colored-env-p (moin--create-dynamic-bg-face (match-string 3))) prepend))
    ;; One line & multiline code environments with other processing instructions
    ("\\({\\{3,4\\}\\)\\(#![a-z]+ [a-z]+?$\\)\\([[:ascii:][:nonascii:]]*?\\)\\(}\\{3,4\\}\\)"
     (1 'moin-face-env-braces prepend)
     (2 'moin-face-env-parser prepend)
     (3 'moin-face-env t)
     (4 'moin-face-env-braces prepend))
    ;; One line & multiline code environments without color spec
    ("\\({\\{3,4\\}\\)\\([^{#][[:ascii:][:nonascii:]]*?\\)\\(}\\{3,4\\}\\)"
     (1 'moin-face-env-braces prepend)
     (2 'moin-face-env t)
     (3 'moin-face-env-braces prepend))
    
    ; ------
    ; Highlighting Links
    ; -------
    ;; Freehand absolute URL
    ("\\(http\\|https\\|ftp\\|nntp\\|news\\|mailto\\|telnet\\|wiki\\|file\\|irc\\)\\(://[A-Za-z0-9_-+&?%#:./=;$]+\\)"
     (1 'moin-face-followable-link t)
     (2 'moin-face-followable-link t))
    ;; EMail
    ("\\([A-Za-z0-9_+-]+@[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+\\)"
     (1 'moin-face-email t))
    ;; Explicit Link
    ("\\(\\[\\[\\)\\(.*?\\)\\(\\]\\]\\)"
     (1 'moin-face-link-brackets t)
     (2 'moin-face-link keep)
     (3 'moin-face-link-brackets t))
    ;; Moin WikiWord    
    ("\\(?:^\\|[^A-Za-z!]\\)\\(\\(?:\\.\\./\\)?/?[A-Z][a-z]+[A-Z][a-z][A-Za-z]*\\(?:/[A-Z][a-z]+[A-Z][a-z][A-Za-z]*\\)?\\)"
     (1 'moin-face-wiki-word t))m
    
    ; ------
    ; Highlighting various other elements
    ; -------
    ;; Horizontal rules
    ("-\\{4,\\}" (0 'moin-face-rule t))
    ;; Inline commentsm
    ("\\(/\\*.*?\\*/\\)"
     (1 'moin-face-comment t))
    ;; Variables
    ("\\(@.*?@\\)"
     (1 'moin-face-variable t))
    ;; Processing instructions
    ("\\(^#.*$\\)"
     (1 'moin-face-processing-instruction t))
    ) 'set))


(defun moin--setup-key-bindings ()
  "Installs all key bindings"

  ;; ;; Headline motion redefined to org key-bindings
  ;; (define-key moin-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
  ;; (define-key moin-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
  ;; (define-key moin-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
  ;; (define-key moin-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
  ;; (define-key moin-mode-map (kbd "C-c C-u") 'outline-up-heading)

  ;; ;; Structure editing redefined to org key-bindings or reassociated
  ;; (define-key moin-mode-map (kbd "C-c @") 'outline-mark-subtree)
  ;; ;; TODO: The following two commands sometimes do not work as expected...
  ;; (global-set-key [M-S-up] 'moin-command-meta-shift-up)
  (define-key moin-mode-map [M-S-up] 'moin-command-meta-shift-up)
  ;; (define-key moin-mode-map (kbd "M-S-<down>") 'outline-move-subtree-down)
  ;; (define-key moin-mode-map [M-up] 'moin-command-meta-shift-up)

  ;; (let ((map outline-mode-map))
  ;; (define-key map (kbd "M-S-<up>") 'moin-command-meta-shift-up)
  ;;   )

  ;; Visibility cycling
  (when (require 'outshine nil t)
    (define-key moin-mode-map [tab] 'moin-command-outline-cycle))

  

  )

;; ==================================================
;; Functions

(defun moin-is-on-heading-p ()
  "Is point on a header line?"
  (outline-on-heading-p))
;  (save-excursion
;    (beginning-of-line)
;    (looking-at "=+ \\(.*\\) =+$")))


;; ==================================================
;; Commands

(defun moin-command-outline-cycle ()
  "Cycles the current heading level between three states, if point is 
currently located on a heading, otherwise delegates to the global binding"
  (interactive)
  (if (moin-is-on-heading-p)
      (progn (outline-cycle))
    ; else
      (call-interactively (global-key-binding "\t"))))

(defun moin-command-meta-shift-up ()
  "Meta command that performs different functions based on the context:
* If point is currently on a heading, it basically does the same thing 
as `outline-move-subtree-up', but without bugs.
* If point is currently in a table, TODO.
* If point is currently in a list, TODO.
"
  (interactive)
  (if (moin-is-on-heading-p)
      (progn (outline-cycle))
      ; else
      (error "Point is currently not on a heading, in a table or in a list")
    ))

;; ==================================================
;; Major mode definition

(define-derived-mode moin-mode text-mode "moin"
  "Set major mode for editing MoinMoin pages"
  (outline-minor-mode)
  
  ; Preparations for outline minor mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "=+ ")
  (make-local-variable 'outline-heading-end-regexp)
  (setq outline-heading-end-regexp " =+$")
  ; Setup related modes
  (toggle-truncate-lines 0)
  (visual-line-mode 1)
  ; Setup moin internal data
  (moin--setup-key-bindings)
  (moin--setup-font-lock)
  )

(add-to-list 'auto-mode-alist '("[0-9]+\\'" . moin-mode))

;; ==================================================
;; Provide package

(provide 'moin-mode)

;;; moin-mode.el ends here
