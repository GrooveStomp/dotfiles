;; Global vars.
(defvar *emacs-load-start* (current-time))

(setq byte-compile-warnings '(cl-functions))

;;------------------------------------------------------------------------------
;; Auto-install package dependencies
;;------------------------------------------------------------------------------
(setq package-list
      '(dirtree
        go-mode
        gruvbox-theme
        highlight-parentheses
        multi-term
        neotree
        org-bullets
        paredit
        terraform-mode
        tramp
        tree-mode
        windata
        zig-mode))

(setq package-archives
      '(("elpa" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
   (unless (package-installed-p package)
     (package-install package)))

;;------------------------------------------------------------------------------
;; Onto config now
;;------------------------------------------------------------------------------

;; Run an Emacs daemon we can reattach to.
(load "server")
(unless (server-running-p) (server-start))

(global-unset-key "\C-h\C-n") ; Emacs news.
(global-unset-key "\C-hn")    ; Emacs news.

(setq-default fill-column 80)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
# Scratch buffer, text-mode..
# Remember, you are awesome. Stick with it!")
(setq ring-bell-function 'ignore)

;; ELPA Package manager setup.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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
;; https://stackoverflow.com/a/5346216
(defadvice replace-string (around turn-off-case-fold-search)
  (let ((case-fold-search nil))
    ad-do-it))
(ad-activate 'replace-string)

(transient-mark-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set file backup scheme.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Make normal search work like dired-isearch-filenames when in dired.
      dired-isearch-filenames t)

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp "~/.emacs-saves"))

;; Dependencies.
(require 'paredit)
;(require 'highlight-parentheses)
(require 'tree-mode)
(require 'windata)
(require 'multi-term)
(require 'tramp)
;(require 'dirtree)
(require 'neotree)
;(require 'auto-complete-exuberant-ctags)
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

; Set the tramp remote connection default type.
(setq tramp-default-method "sshx")

; Enable auto-complete.
;(ac-exuberant-ctags-setup)
(setq multi-term-program (getenv "SHELL"))
;(auto-complete-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "4b19d61c560a93ef90767abe513c11f236caec2864617d718aa366618133704c" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "80ae3a89f1eca6fb94a525004f66b544e347c6f756aaafb728c7cdaef85ea1f5" "3e83abe75cebf5621e34ce1cbe6e12e4d80766bed0755033febed5794d0c69bf" "fc0c179ce77997ecb6a7833310587131f319006ef2f630c5a1fec1a9307bff45" "227edf860687e6dfd079dc5c629cbfb5c37d0b42a3441f5c50873ba11ec8dfd2" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "5dd70fe6b64f3278d5b9ad3ff8f709b5e15cd153b0377d840c5281c352e8ccce" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" default)))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (bison-mode gemini-mode systemd nginx-mode treemacs neotree projectile markdown-mode terraform-mode prettier-js org-time-budgets svelte-mode string-inflection solarized-theme zig-mode yaml-mode birds-of-paradise-plus-theme gruvbox-theme autumn-light-theme org-bullets mellow-theme darktooth-theme labburn-theme rust-mode borland-blue-theme d-mode sublime-themes railscasts-theme color-theme-tango go-mode color-theme-monokai highlight-parentheses paredit)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#32302f")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25 t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

;; Custom functions.
(load-library "~/.emacs.d/support.el")

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
(global-set-key (kbd "M-Q")     'unfill-paragraph)

;; Enable/Disable font-lock. (Syntax highlighting.)
(global-font-lock-mode 1)

;; Theme
(load-theme 'gruvbox-dark-soft t)

;; GUI Options.
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      ))

(menu-bar-mode -1)
(set-face-attribute 'default nil :font "Liberation Mono 16")

;; Configure file extensions with specific modes.
(mapcar
 (lambda (file-ext) (add-to-list 'auto-mode-alist `(,file-ext . lisp-mode)))
 '("\\.emacs$" "\\.cl$" "\\.asd$" "\\.el$"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; Not needed since Emacs 22.2?

;; Finish calculating total load time for .emacs.
(defvar *finish-time* (current-time))
(message "My .emacs loaded in %fs"
         (let (finish-time (current-time))
           (- (+ (car *finish-time*)
                 (cadr *finish-time*))
              (+ (car *emacs-load-start*)
                 (cadr *emacs-load-start*)))))
(put 'narrow-to-region 'disabled nil)

;;------------------------------------------------------------------------------
;; Major Mode Hooks
;;------------------------------------------------------------------------------

(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(add-hook 'go-mode-hook
          (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux")
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

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode -1)
            (org-indent-mode 1)
            (local-set-key "\C-cl" 'org-store-link)
            (local-set-key "\C-ca" 'org-agenda)
            (local-set-key "\C-cb" 'org-iswitchb)
            (setq org-log-done 'time)
            (setq org-todo-keywords
                  '((sequence "TODO" "STARTED" "WAITING" "DEFERRED" "|" "DONE" "CANCELLED")))
            ;;(setq org-log-done t)
            (setq org-ellipsis " ▼")
            (setq org-src-fontify-natively t)
            (setq org-src-tab-acts-natively t)
            (setq org-src-window-setup 'current-window)
            (load-library "find-lisp")
            ;; Get initial list of agenda files.
            (setq aaron-agenda-files (find-lisp-find-files "~/notes" "\.org$"))
            ;; Filter out syncthing backed-up files.
            (setq aaron-agenda-files
                  (seq-remove (lambda (path)
                                (string-match "\.stversions" path))
                              aaron-agenda-files))
            ;; DEBUG: Show value of aaron-agenda-files
            ;; (mapcar (lambda (path) (message path)) aaron-agenda-files)
            (setq org-agenda-files aaron-agenda-files)))
