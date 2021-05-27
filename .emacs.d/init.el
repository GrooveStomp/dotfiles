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

(global-unset-key "\C-h\C-n") ; Disable emacs news
(global-unset-key "\C-hn") ; Disable emacs news

(global-set-key (kbd "M-q")     'fill-paragraph)
(global-set-key (kbd "M-n")     'forward-paragraph)
(global-set-key (kbd "M-p")     'backward-paragraph)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(transient-mark-mode t)
(global-font-lock-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default nil :font "Liberation Mono 13")
(put 'narrow-to-region 'disabled nil)
(show-paren-mode t)

(setq-default column-number-mode t ; Show column numbesr
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

(use-package seq
  :straight (:type built-in))
(use-package find-lisp)
(use-package org)
(use-package org-bullets
  :requires (org find-lisp)
  :config
  (org-bullets-mode -1)
  (org-indent-mode 1)
  (local-set-key "\C-cl" 'org-store-link)
  (local-set-key "\C-ca" 'org-agenda)
  (local-set-key "\C-cb" 'org-iswitchb)
  (setq org-log-done 'time
        org-todo-keywords
        '((sequence "TODO" "STARTED" "WAITING" "DEFERRED" "|" "DONE" "CANCELLED"))
        org-ellipsis " ▼"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        ; Get initial list of agenda files.
        aaron-agenda-files-unfiltered (find-lisp-find-files "~/notes" "\.org$")
        aaron-agenda-files (seq-remove (lambda (path)
                                         (string-match "\.stversions" path))
                                       aaron-agenda-files-unfiltered)
        org-agenda-files aaron-agenda-files))
(use-package magit)
(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :hook (before-save . gofmt-before-save))
(use-package highlight-parentheses)
(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))
(use-package windata)
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
(use-package smtpmail)
(use-package mu4e
  :straight (:type built-in))
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
(use-package my-mu4e
  :requires (mu4e smtpmail)
  :straight nil
  :load-path "~/.emacs.d/packages/"
  :hook (mu4e-compose-pre . my-mu4e-set-account))
(use-package mastodon
  :straight (:type git :host github :repo "jdenen/mastodon.el")
  :config (setq mastodon-instance-url "https://social.linux.pizza"))

;; Site-local configuration.

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))
