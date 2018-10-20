;;; Configuration for message-mode
;;; https://www.emacswiki.org/emacs/GnusMSMTP
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
;(setq message-sendmail-extra-arguments '("-a" "myaccount")) ; not needed after msmtp 1.4.31
;(setq user-full-name "Firstname Lastname")
;(setq user-mail-address "firstname.lastname@example.com")
