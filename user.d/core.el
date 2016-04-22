;;;
;;; Basic settings
;;;

;; Auto-save path
(defconst my-temp-dir "~/tmp/emacs")
(make-directory my-temp-dir t)
(setq backup-directory-alist
      `((".*" . ,my-temp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-temp-dir t)))

;; No splash screen messages
(setq inhibit-startup-message t)

;; Silence description of the *scratch* buffer
(setq initial-scratch-message "")

;; Accept simply 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display cursor line/column number in modeline
(column-number-mode)

;; ffap (find-file-at-point)
;; From: http://stackoverflow.com/questions/259354/goto-file-in-emacs
;; Replace C-x C-f and others with ffap versions, ala vim's gf command.
(ffap-bindings)

;; Remove UI elements from GUI mode
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Show un/matched parens
(show-paren-mode 1)      
(setq show-paren-delay 0)

;; Show empty lines and indicate start/end of buffer
;; (setq-default indicate-empty-lines t)
;; (setq-default indicate-buffer-boundaries t)

(set-frame-font "Terminus-11" nil t)
;;(set-frame-font "Monospace-12" nil t)
;;(set-frame-font "snap-12" nil t)
;;(set-frame-font "Monospace-12" nil t)
;;(set-face-attribute 'default nil :height 100)
  
;; Resizing fonts
;; C-x C-= to increase
;; C-x C-- to decrease

