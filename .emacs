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

;; Always follow symlinks.
(setq vc-follow-symlinks t)
;; General Emacs settings.
(set-default 'truncate-lines t)
(setq-default indent-tabs-mode nil
              tab-width 2)
(setq inhibit-startup-screen t ; Disable splash screen.
      column-number-mode t ; Show column numbers
      indent-line-function 'insert-tab)

;; Case-sensitive query-replace.
(defadvice replace-string (around turn-off-case-fold-search)
  (let ((case-fold-search nil))
    ad-do-it))

(ad-activate 'replace-string)

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
      ;; Make normal search work like dired-isearch-filenames when in dired.
      dired-isearch-filenames t
      inferior-lisp-program "/usr/bin/sbcl")

;; Dependencies.
(require 'paredit)
;(require 'highlight-parentheses)
(require 'tree-mode)
(require 'windata)
(require 'multi-term)
(require 'tramp)
(require 'dirtree)
;(require 'auto-complete-exuberant-ctags)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

; Set the tramp remote connection default type.
(setq tramp-default-method "sshx")

; Enable auto-complete.
;(ac-exuberant-ctags-setup)
(setq multi-term-program "/bin/bash")
;(auto-complete-mode)

(defun .add-to-lisp-mode (file-ext)
  (add-to-list 'auto-mode-alist `(,file-ext . lisp-mode)))
(mapcar '.add-to-lisp-mode '("\\.emacs$" "\\.cl$" "\\.asd$" "\\.el$"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-mode color-theme-monokai highlight-parentheses paredit))))

;; Custom functions.
(load-library "~/.emacs.d/support.el")
; (load-library "lisp-config.el")

(windmove-default-keybindings)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Bright-red TODOs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode ruby-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

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

(setq c-default-style "linux")
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq tab-width 8)
            (setq c-basic-offset 8)
            ;; Make `case' statements in `switch' blocks indent normally.
            (c-set-offset 'case-label '+)))

;(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono 12") ; 14
;(set-face-attribute 'default nil :font "Deja Vu Sans Mono 13")
;; 4K: 14, 1440P: 12
;(set-face-attribute 'default nil :font "Fantasque Sans Mono 14")
(set-face-attribute 'default nil :font "Liberation Mono 13") ; 14
;(set-face-attribute 'default nil :font "Source Sans Pro 13")

;; Color Themes
                                        ;
;(color-theme-railscasts)
;(color-theme-solarized)
(color-theme-molokai)
;(color-theme-monokai)

;; Finish calculating total load time for .emacs.
(defvar *finish-time* (current-time))
(message "My .emacs loaded in %ds"
         (let (finish-time (current-time))
           (- (+ (first *finish-time*)
                 (second *finish-time*))
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
(put 'narrow-to-region 'disabled nil)
