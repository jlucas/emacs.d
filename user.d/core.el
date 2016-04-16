;;;
;;; Basic settings
;;;

;; Auto-save path
(defconst my-temp-dir "~/tmp")
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

;; http://unix.stackexchange.com/questions/79374
(defun jl/terminal-setup (&optional frame)
  ;; You need this in order to make the hook run on the correct frame.
  ;; This is important when running emacsclient in a terminal under an
  ;; emacs server that was started in a graphical environment.
  ;; http://stackoverflow.com/questions/7616761
  (select-frame frame)
  ;; non-nil when emacs is started graphically
  (message (format "window-system is %s" window-system))
  ;; nil when emacs is started graphically
  (message (format "display-graphic-p is %s" (display-graphic-p)))
  (unless (display-graphic-p)
    (message "running my-terminal-config...")
    (xterm-mouse-mode t)
    ;; From: http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
    (define-key input-decode-map "\e[1;5A" [C-up])
    (define-key input-decode-map "\e[1;5B" [C-down])
    (define-key input-decode-map "\e[1;5C" [C-right])
    (define-key input-decode-map "\e[1;5D" [C-left])
    (define-key input-decode-map "\e[13;5u" [(control return)])
    (define-key input-decode-map "\e[1;7C" [C-M-right])
    (define-key input-decode-map "\e[1;7D" [C-M-left])
    (define-key input-decode-map "\e[1;7A" [C-M-up])
    (define-key input-decode-map "\e[1;7B" [C-M-down])
    (define-key input-decode-map "\e[13;5u" [(control return)])))

(add-hook 'after-make-frame-functions 'jl/terminal-setup)

