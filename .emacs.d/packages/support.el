;; dirty fix for having AC everywhere

;; (define-globalized-minor-mode real-global-auto-complete-mode
;;     auto-complete-mode (lambda ()
;;                          (if (not (minibufferp (current-buffer)))
;;                              (auto-complete-mode 1))
;;                          ))
;; (real-global-auto-complete-mode t)

(defun support-column-guide-show (&optional col)
  "Highlights the given column"
  (interactive)
  (column-marker-1 col))

(defun support-column-guide-hide ()
  "Hides the column guide"
  (interactive)
  (column-marker-1 -1))

(defun support-get-point (symbol &optional arg)
  "Get the point"
  (funcall symbol arg)
  (point))

(defun support-copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun support-paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe(lambda()
                  (if (string= "shell-mode" major-mode)
                      (progn (comint-next-prompt 25535) (yank))
                    (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))))

(defun support-copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg))

(defun support-copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg))

(defun support-copy-paragraph (&optional arg)
  "Copy paragraphes at point"
  (interactive "P")
  (copy-thing 'backward-paragraph 'forward-paragraph arg))

(defun support-beginning-of-string(&optional arg)
  "  "
  (re-search-backward "[ \t]" (line-beginning-position) 3 1)
  (if (looking-at "[\t ]")  (goto-char (+ (point) 1)) ))

(defun support-end-of-string(&optional arg)
  " "
  (re-search-forward "[ \t]" (line-end-position) 3 arg)
  (if (looking-back "[\t ]") (goto-char (- (point) 1))))

(defun support-thing-copy-string-to-mark(&optional arg)
  "Try to copy a string and paste it to the mark When used in shell-mode, it will paste string on shell prompt
   by default"
  (interactive "P")
  (copy-thing 'beginning-of-string 'end-of-string arg)
  (paste-to-mark arg))

(defun support-beginning-of-parenthesis(&optional arg)
  "  "
  (re-search-backward "[[<(?\"]" (line-beginning-position) 3 1)
  (if (looking-at "[[<(?\"]")  (goto-char (+ (point) 1)) ))

(defun support-end-of-parenthesis(&optional arg)
  " "
  (re-search-forward "[]>)?\"]" (line-end-position) 3 arg)
  (if (looking-back "[]>)?\"]") (goto-char (- (point) 1)) ))

(defun support-thing-copy-parenthesis-to-mark(&optional arg)
  "Try to copy a parenthesis and paste it to the mark When used in shell-mode, it will paste parenthesis on
   shell prompt by default"
  (interactive "P")
  (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
  (paste-to-mark arg))

;; Behave like vi's o command.
(defun support-open-next-line (arg)
  "Move to the next line and then open a line.  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when #'newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command.
(defun support-open-previous-line (arg)
  "Open a new line before the current one.  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when #'newline-and-indent
    (indent-according-to-mode)))

;; Delete all whitespace from point to next word.
(defun support-delete-horizontal-space-forward ()
  "Delete all spaces and tabs after point."

  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

;; Toggle between vertical and horizontal split if there are exact two windows showing.
(defun support-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun support-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(provide 'support)
