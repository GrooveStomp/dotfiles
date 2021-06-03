(setq mu4e-mu-binary (locate-file "mu" exec-path)
      sendmail-program (locate-file "msmtp" exec-path)
      send-mail-function 'smtpmail-send-it
      mu4e-sent-folder "/disroot/Saved Items"
      mu4e-drafts-folder "/disroot/Drafts"
      mu4e-trash-folder "/disroot/Trash"
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-user-agent 'mu4e-user-agent
      user-mail-address "groovestomp@disroot.org"
      user-full-name "GrooveStomp"
      mu4e-get-mail-command "mbsync -a"
      mu4e-compose-reply-to-address "groovestomp@disroot.org")

(defvar my-mu4e-account-alist
  '(("mogo"
     (mu4e-sent-folder "/mogo/Saved Items")
     (mu4e-drafts-folder "/mogo/Drafts")
     (mu4e-trash-folder "/mogo/Trash")
     (mu4e-refile-folder "/mogo/Archive")
     (user-mail-address "aaron.oman@mogo.ca")
     (user-full-name "Aaron Oman")
     (mu4e-get-mail-command "mbsync mogo")
     (mu4e-compose-reply-to-address "aaron.oman@mogo.ca"))

    ("disroot"
     (mu4e-sent-folder "/disroot/Saved Items")
     (mu4e-drafts-folder "/disroot/Drafts")
     (mu4e-trash-folder "/disroot/Trash")
     (mu4e-refile-folder "/disroot/Archive")
     (user-mail-address "groovestomp@disroot.org")
     (user-full-name "GrooveStomp")
     (mu4e-get-mail-command "mbsync disroot")
     (mu4e-compose-reply-to-address "groovestomp@disroot.org"))

    ("aarondee"
     (mu4e-sent-folder "/aarondee/Saved Items")
     (mu4e-drafts-folder "/aarondee/Drafts")
     (mu4e-trash-folder "/aarondee/Trash")
     (mu4e-refile-folder "/aarondee/Archive")
     (user-mail-address "aaron.oman@aarondee.ca")
     (user-full-name "Aaron & Daniela")
     (mu4e-get-mail-command "mbsync aarondee")
     (mu4e-compose-reply-to-address "aarondee2011@gmail.com"))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(defun my-mu4e-set-from ()
  "Set the From address based on the To address."
  (let ((msg mu4e-compose-parent-message))
    (when msg
      (setq user-mail-address
            (cond
             ((mu4e-message-contact-field-matches msg :to "aaron.oman@mogo.ca")
              "aaron.oman@mogo.ca")
             ((mu4e-message-contact-field-matches msg :to "aaron@aaronoman.com")
              "aaron@aaronoman.com")
             ((mu4e-message-contact-field-matches msg :to "@groovestomp.com")
              "groovestomp@groovestomp.com")
             ((mu4e-message-contact-field matches msg :to "groovestomp@disroot.org")
              "groovestomp@groovestomp.com")
             ((mu4e-message-contact-field matches msg :to "aaron.oman.*@gmail.com")
              "groovestomp@groovestomp.com"))))))

(provide 'my-mu4e)
