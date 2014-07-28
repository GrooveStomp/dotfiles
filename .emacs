(require 'cl)

;; ELPA Package manager setup.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Global vars.
(defvar *emacs-load-start* (current-time))
(defvar *home* "/home/aoman/")
(defvar *emacs-saves* (concat *home* ".emacs-saves/"))
(defvar *emacs-root* (concat *home* ".emacs.d/"))

;; General Emacs settings.
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq inhibit-startup-screen t ; Disable splash screen.
      column-number-mode t ; Show column numbers
      indent-line-function 'insert-tab)

(global-font-lock-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(transient-mark-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file backup scheme.
(setq backup-by-copying t
      backup-directory-alist `(("." . ,*emacs-saves*))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      inferior-lisp-program "/usr/bin/sbcl")

;; Dependencies.
(add-to-list 'load-path *emacs-root*)
(require 'paredit)
(require 'highlight-parentheses)
;(require 'slime-autoloads)
(require 'tree-mode)
(require 'windata)
(require 'dirtree)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(global-set-key "\C-o" 'dirtree-show)
; Set goto-line key shortcut.
(global-set-key "\C-x\C-l" 'goto-line)

; Set the font face.
(set-face-attribute 'default t :font "Ubuntu Mono 12")

(defun .add-to-lisp-mode (file-ext)
  (add-to-list 'auto-mode-alist `(,file-ext . lisp-mode)))
(mapcar '.add-to-lisp-mode '("\\.emacs$" "\\.cl$" "\\.asd$" "\\.el$"))

(defun .set-color-theme ()
  (color-theme-initialize)
  (color-theme-tango))

(add-hook 'after-init-hook
          (lambda ()
            (.set-color-theme)
            (setq slime-complete-symbol*-fancy t
                  slime-complete-symbol-function 'slime-fuzzy-complete-symbol
                  slime-when-complete-filename-expand t
                  slime-truncate-lines nil
                  slime-autodoc-use-multiline-p t)
            (slime-setup '(slime-fancy slime-asdf))
            (define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
            (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (highlight-parentheses-mode t)
            (paredit-mode t)
            (setq hl-paren-colors '("red1" "cyan1" "yellow1" "green1" "cyan1" "slateblue1" "magenta1" "purple"))
            (slime-mode t)))

;; Finish calculating total load time for .emacs.
(defvar *finish-time* (current-time))
(message "My .emacs loaded in %ds"
         (let (finish-time (current-time))
           (- (+ (first *finish-time*)
                 (second *finish-time*))
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
