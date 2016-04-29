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
  
;; Resizing fonts
;; C-x C-= to increase
;; C-x C-- to decrease

;; Fullscreen in X with f11 key
;; From: http://emacswiki.org/emacs/FullScreen#toc22
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [f11] 'fullscreen)

;;; Mouse scrolling and pointing in terminal mode
(require 'xt-mouse)
(defun up-slightly () (interactive) (scroll-up 2))
(defun down-slightly () (interactive) (scroll-down 2))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

;; http://unix.stackexchange.com/questions/79374
;; https://www.emacswiki.org/emacs/MetaKeyProblems#toc16
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
    (message "running terminal setup...")
    (xterm-mouse-mode t)
    ;; From: http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
    (define-key input-decode-map "\e[1;2C" [S-right])
    (define-key input-decode-map "\e[1;2D" [S-left])
    (define-key input-decode-map "\e[1;2A" [S-up])
    (define-key input-decode-map "\e[1;2B" [S-down])
    (define-key input-decode-map "\e[1;5A" [C-up])
    (define-key input-decode-map "\e[1;5B" [C-down])
    (define-key input-decode-map "\e[1;5C" [C-right])
    (define-key input-decode-map "\e[1;5D" [C-left])
    (define-key input-decode-map "\e[1;7C" [C-M-right])
    (define-key input-decode-map "\e[1;7D" [C-M-left])
    (define-key input-decode-map "\e[1;7A" [C-M-up])
    (define-key input-decode-map "\e[1;7B" [C-M-down])
    (define-key input-decode-map "\e[13;5u" [(control return)])))
(add-hook 'after-make-frame-functions 'jl/terminal-setup)
