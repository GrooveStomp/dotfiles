;; Setup straight.el

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Now on towards the regular configuration.

(define-key global-map (kbd "C-h C-n") nil) ; Disable emacs news
(define-key global-map (kbd "C-h n") nil) ; Disable emacs news

(define-key global-map (kbd "M-q") 'fill-paragraph)
(define-key global-map (kbd "M-n") 'forward-paragraph)
(define-key global-map (kbd "M-p") 'backward-paragraph)
(define-key global-map (kbd "C-x C-b") 'ibuffer-other-window)

(desktop-save-mode 1)
(windmove-default-keybindings)
(transient-mark-mode t)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :font "Liberation Mono 13")
(put 'narrow-to-region 'disabled nil)
(show-paren-mode t)

;; Only run this elisp when we're using a gui.
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(setq-default column-number-mode t ; Show column numbers.
              indent-line-function 'insert-tab
              indent-tabs-mode nil
              inhibit-startup-screen t ; Disable splash screen
              package-enable-at-startup nil ; Disable package.el
              ring-bell-function 'ignore ; Disable visual bell
              tab-width 2
              truncate-lines t ; Display only the portion of lines that fits within fill-column
              vc-follow-symlinks t
              fill-column 110 ; M-q autowraps text at 110 characters.
              backup-by-copying t
              backup-directory-alist '(("." . "~/.emacs-saves"))
              delete-old-versions t
              kept-new-versions 6
              kept-old-version 2
              version-control t
              dired-isearch-filenames t
              debug-on-error t ; debugging
              straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package tramp
  :straight nil
  :config
  (setq tramp-default-method "sshx")
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp "~/.emacs-saves")))
(use-package seq
  :straight (:type built-in))
(use-package org
  :mode ("\\.org$" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (org-indent-mode 1)
  (setq org-log-done 'time
        org-todo-keywords
        '((sequence "TODO" "STARTED" "WAITING" "DEFERRED" "|" "DONE" "CANCELLED"))
        org-ellipsis " ▼"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-agenda-files '("~/notes/agenda.org"
                           "~/notes/mogo.org")))
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
(use-package magit)
(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :hook (before-save . gofmt-before-save))
(use-package highlight-parentheses)
(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))
(use-package zig-mode
  :mode ("\\.zig$" . zig-mode))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))
(use-package js
  :mode (("\\.js$" . js-mode) ("\\.json$" . js-mode))
  :straight (:type built-in)
  :config (setq js-indent-level 2))
(use-package cc-mode
  :straight (:type built-in)
  :config
  (setq tab-width 8
        c-basic-offset 8)
  (c-set-offset 'case-label '+)) ; Make case statements in switch blocks indent nicely.
(use-package lisp-mode
  :straight (:type built-in)
  :mode (("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode) ("\\.asd$" . lisp-mode)))
(use-package elisp-mode
  :straight (:type built-in)
  :mode (("\\.emacs$" . emacs-lisp-mode) ("\\.el$" . emacs-lisp-mode)))
(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode))
  :config (define-key paredit-mode-map "\M-q" nil))
(use-package simple
  :straight (:type built-in)
  :demand t
  :hook (before-save . delete-trailing-whitespace)
  :config (global-set-key (kbd "RET") 'newline-and-indent))
(use-package support
  :requires (simple)
  :straight nil
  :load-path "~/.emacs.d/packages/"
  :functions newline-and-indent
  :config
  (global-set-key (kbd "C-o")     'support-open-next-line)
  (global-set-key (kbd "M-o")     'support-open-previous-line)
  (global-set-key (kbd "M-\\")    'support-delete-horizontal-space-forward)
  (global-set-key (kbd "C-c C-w") 'support-copy-word)
  (global-set-key (kbd "C-c C-l") 'support-copy-line)
  (global-set-key (kbd "C-c C-p") 'support-copy-paragraph)
  (global-set-key (kbd "M-Q")     'support-unfill-paragraph))
(use-package smtpmail)
(use-package mu4e ; Only load this way for OSX
  :if (memq window-system '(mac ns x))
  :straight nil
  :load-path "/usr/local/Cellar/mu/1.4.15/share/emacs/site-lisp/mu/mu4e/"
  :config (setq mu4e-change-filenames-when-moving t))
(use-package mu4e ; Load this way for every other OS
  :if (not (memq window-system '(mac ns x)))
  :straight nil)
(use-package mu4e-maildirs-extension
  :requires (mu4e))
(use-package my-mu4e
  :requires (mu4e smtpmail)
  :straight nil
  :load-path "~/.emacs.d/packages/"
  :hook (mu4e-compose-pre . my-mu4e-set-account))
(use-package mastodon
  :config (setq mastodon-instance-url "https://social.linux.pizza"))
(use-package erc
  :straight nil
  :config
  (setq erc-nick "groovestomp"
        erc-user-full-name "GrooveStomp"))

;; Site-local configuration.

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))
