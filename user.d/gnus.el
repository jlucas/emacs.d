;; ;;;; RECEIVE
;; (setq gnus-secondary-select-methods
;;       '((nnimap "neoprimitive.net"
;;                 (nnimap-address "mail.0x98.net")
;;                 (nnimap-server-port 993)
;;                 (nnimap-authenticator login)
;;                 (nnimap-expunge-on-close 'never)
;;                 (nnimap-stream ssl))))

;; ;;;; SEND
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("mail.0x98.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("mail.0x98.com" 587 "jl@0x98.com" nil))
;;       smtpmail-default-smtp-server "mail.0x98.com"
;;       smtpmail-smtp-server "mail.0x98.com"
;;       smtpmail-smtp-service 587
;;       mail-host-address "jl@0x98.com")

(setq gnus-secondary-select-methods nil)

(setq gnus-select-method
      '(nnmaildir "Mail"
		  (directory "/home/jlucas/.cache/offlineimap")
		  (expire-age never)))

(setq gnu-fetch-old-headers t)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

;;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Checking-New-Groups.html#Checking-New-Groups
;;; (setq gnus-check-new-newsgroups ask-server)

;;; https://www.gnu.org/software/emacs/manual/html_node/gnus/The-Active-File.html#The-Active-File
;;; "If you think that starting up Gnus takes too long, try all the
;;; three different values for this variable and see what works best
;;; for you."

;; (setq gnus-extract-address-components)


