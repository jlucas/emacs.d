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

;;; Preserve history across sessions
;;; http://stackoverflow.com/questions/1229142
(setq savehist-file (concat (file-name-as-directory my-temp-dir) "history"))
(savehist-mode 1)

;; Turn on automatic bracket insertion by pairs.  New in Emacs 24.
(if (>= emacs-major-version 24)
    (electric-pair-mode 1))

;; Show un/matched parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Use PRIMARY selection in X
;; From: http://emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-primary t)

;; Include newline when killing a line
(setq kill-whole-line t)

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

; Line numbers
(setq linum-format "%3d ")

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

