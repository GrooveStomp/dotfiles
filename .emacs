(require 'cl)

;; ELPA Package manager setup.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Global vars.
(defvar *emacs-load-start* (current-time))

;; General Emacs settings.
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq inhibit-startup-screen t ; Disable splash screen.
      column-number-mode t ; Show column numbers
      indent-line-function 'insert-tab)

;; GUI Options.
(global-font-lock-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(transient-mark-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file backup scheme.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      inferior-lisp-program "/usr/bin/sbcl")

;; Dependencies.
(add-to-list 'load-path "~/.emacs.d")
(require 'paredit)
(require 'highlight-parentheses)
(require 'tree-mode)
(require 'windata)
(require 'multi-term)
(require 'tramp)
(require 'dirtree)
(require 'auto-complete-exuberant-ctags)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; Color Themes
;; NOTE(AARON): Only require color themes I'm actually using.
;; These are just here for whenever I want to try them out and try something different.
; (require 'color-theme-molokai)
; (require 'color-theme-monokai)
; (require 'color-theme-railscasts)
; (require 'color-theme-solarized)
(require 'color-theme-tango)
(color-theme-tango)

; Set the tramp remote connection default type.
(setq tramp-default-method "sshx")

; Set the font face.
;(set-face-attribute 'default t :font "Ubuntu Mono 12")
(set-face-attribute 'default t :font "Liberation Mono 10")

; Enable auto-complete.
(ac-exuberant-ctags-setup)
(setq multi-term-program "/bin/bash")
(auto-complete-mode)

(defun .add-to-lisp-mode (file-ext)
  (add-to-list 'auto-mode-alist `(,file-ext . lisp-mode)))
(mapcar '.add-to-lisp-mode '("\\.emacs$" "\\.cl$" "\\.asd$" "\\.el$"))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'clojure-mode-hook 'paredit-mode)

(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(custom-set-variables '(haskell-tags-on-save t))

;; Custom functions.
(load-library "support.el")
; (load-library "lisp-config.el")

(windmove-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "red" "chartreuse2" "yellow" "light sky blue" "plum3" "cyan" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-blue ((t (:background "light sky blue" :foreground "light sky blue"))))
 '(term-color-green ((t (:background "chartreuse2" :foreground "chartreuse2"))))
 '(term-color-magenta ((t (:background "plum3" :foreground "plum3")))))

;; ;; Org-Mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; Not needed since Emacs 22.2?
(add-hook 'org-mode-hook 'turn-on-font-lock) ; Not needed when global-font-lock-mode is on?
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "C-o")     'open-next-line)
(global-set-key (kbd "M-o")     'open-previous-line)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key (kbd "C-x C-o") 'dirtree-show)
(global-set-key (kbd "M-\\")    'delete-horizontal-space-forward)
(global-set-key (kbd "C-c C-w") 'copy-word)
(global-set-key (kbd "C-c C-l") 'copy-line)
(global-set-key (kbd "C-c C-p") 'copy-paragraph)
(global-set-key (kbd "M-n")     'forward-paragraph)
(global-set-key (kbd "M-p")     'backward-paragraph)

;; Finish calculating total load time for .emacs.
(defvar *finish-time* (current-time))
(message "My .emacs loaded in %ds"
         (let (finish-time (current-time))
           (- (+ (first *finish-time*)
                 (second *finish-time*))
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
