(require 'cl)

(dolist (key '("\C-h\C-n" ;; Disable shortcut to Emacs news.
               "\C-hn"))  ;; Disable shortcut to Emacs news.
  (global-unset-key key))

(setq-default fill-column 80)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# Scratch buffer, text-mode..
# Remember, you are awesome. Stick with it!")

;; ELPA Package manager setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
 '(custom-safe-themes
   (quote
    ("fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" "227edf860687e6dfd079dc5c629cbfb5c37d0b42a3441f5c50873ba11ec8dfd2" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "5dd70fe6b64f3278d5b9ad3ff8f709b5e15cd153b0377d840c5281c352e8ccce" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" default)))
 '(package-selected-packages
   (quote
    (org-bullets mellow-theme darktooth-theme labburn-theme rust-mode borland-blue-theme d-mode sublime-themes railscasts-theme color-theme-tango go-mode color-theme-monokai highlight-parentheses paredit))))

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

;;;
;;; Org-Mode
;;;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; Not needed since Emacs 22.2?
(add-hook 'org-mode-hook 'turn-on-font-lock) ; Not needed when global-font-lock-mode is on?
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "DONE" "CANCELLED" "DEFERRED")))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (quote ("~/notes")))


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

(add-hook 'sh-mode-common-hook
          (lambda ()
            (setq tab-width 8)
            (setq c-basic-offset 8)
            ;; Make `case' statements in `switch' blocks indent normally.
            (c-set-offset 'case-label '+)))

;; Color Themes
;; https://github.com/owainlewis/emacs-color-themes
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/sublime-themes-20160111.122/")
(if (display-graphic-p)
    (load-theme 'mellow t)
  (load-theme 'labburn t))
(set-face-attribute 'default nil :font "Liberation Mono 12")

;; Finish calculating total load time for .emacs.
(defvar *finish-time* (current-time))
(message "My .emacs loaded in %ds"
         (let (finish-time (current-time))
           (- (+ (first *finish-time*)
                 (second *finish-time*))
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))
(put 'narrow-to-region 'disabled nil)

;; Go-mode
(add-hook 'go-mode-hook
          (add-hook 'before-save-hook 'gofmt-before-save))


;; Org-mode
(add-hook 'org-mode-hook
          (lambda () (progn (org-bullets-mode -1)
                            (org-indent-mode 1))))
(setq org-ellipsis " ▼")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
