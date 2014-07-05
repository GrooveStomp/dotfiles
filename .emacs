(require 'cl)

;; Custom functions.
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

;; Key bindings.
(global-set-key (kbd "C-x p") 'goto-match-paren)
(global-set-key (kbd "C-c p") 'goto-match-paren)
(global-set-key (kbd "C-x v") 'yank) ; Set "paste" to "C-x v".
(global-set-key (kbd "C-c v") 'yank) ; Set "paste" to "C-c v".
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; Alternative to M-x.
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; Alternative to M-x.
(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-x C-l") 'goto-line)
(global-set-key (kbd "RET") 'newline-and-indent)

;; General Emacs settings.
(setq x-alt-keysym 'meta)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(transient-mark-mode t)
(set-default 'truncate-lines t) ; Disable line wrapping.
(setq inhibit-startup-screen t) ; Disable splash screen.
(setq column-number-mode t) ; Show column numbers.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(global-font-lock-mode 1)
(menu-bar-mode -1)
(if (memq window-system '(x w32 mac darwin gnu/linux ns))
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;; ELPA Package manager setup.
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Global vars.
(defvar *emacs-load-start* (current-time))
(defvar *home* "/home/aoman/")
(defvar *emacs-saves* (concat *home* ".emacs-saves/"))
(defvar *emacs-root* (concat *home* ".emacs.d/"))

;; Set file backup scheme.
(setq backup-by-copying t
      backup-directory-alist `(("." . ,*emacs-saves*))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Dependencies.
(add-to-list 'load-path *emacs-root*)
(require 'paredit)
(require 'highlight-parentheses)
(require 'slime-autoloads)

(eval-after-load "color-theme" 'color-theme-solarized-dark)
;(add-hook 'color-theme 'color-theme-solarized-dark)

;;
;; Language settings.
;;

;; Lisp
(add-to-list 'auto-mode-alist '("\\.emacs$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.el$" . lisp-mode))
(add-hook 'lisp-mode-hook (lambda ()
			    (enable-paredit-mode)
			    ;; Rebind 'return' key from 'newline' to 'newline-and-indent'.
			    (local-set-key (kbd "RET") 'newline-and-indent)
			    (highlight-parentheses-mode t)
			    (paredit-mode t)
			    (setq hl-paren-colors '("red1" "cyan11" "yellow1" "green1" "cyan1" "slateblue1" "magenta1" "purple"))
			    (slime-mode t)))

;; Slime setup.
(setq inferior-lisp-program
      (cond ((eq 'windows-nt system-type) "wx86cl64.exe")
            (t "/usr/local/bin/sbcl")))
(if (eq 'darwin system-type)
    (setq slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl" "--core" "/Users/aoman/.sbcl-core-for-slime")))))
(setq comomn-lisp-hyperspec-root (concat "file:" *home* ".hyperspec/current/HyperSpec/"))

(slime-setup)
(eval-after-load "slime"
  '(progn (setq
           slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-when-complete-filename-expand t
           slime-truncate-lines nil
           slime-autodoc-use-multiline-p t)
	  (slime-setup '(slime-fancy slime-asdf))
	  (define-key slime-repl-mode-map (kbd "C-c ;") 'slime-insert-balanced-comments)
	  (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)))
(add-hook 'lisp-mode-hook (lambda ()
			    (cond ((not (featurep 'slime))
				   (require 'slime)
				   (normal-mode)))
			    (setq-default indent-tabs-mode nil)))

;; Finish timing Emacs config startup.
(message "My .emacs loaded in %ds"
         (destructuring-bind
               (hi lo ms) (current-time) (- (+ hi lo)
                                            (+ (first *emacs-load-start*)
                                               (second *emacs-load-start*)))))
