
(defun jl/load-if-readable (filepath)
  (if (file-readable-p filepath)
      (load filepath)))

;;; Mouse scrolling and pointing in terminal mode
(require 'xt-mouse)
(defun up-slightly () (interactive) (scroll-up 2))
(defun down-slightly () (interactive) (scroll-down 2))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

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
    ;; (define-key input-decode-map "\e[1;5A" [C-up])
    ;; (define-key input-decode-map "\e[1;5B" [C-down])
    ;; (define-key input-decode-map "\e[1;5C" [C-right])
    ;; (define-key input-decode-map "\e[1;5D" [C-left])
    ;; (define-key input-decode-map "\e[13;5u" [(control return)])
    ;; (define-key input-decode-map "\e[1;7C" [C-M-right])
    ;; (define-key input-decode-map "\e[1;7D" [C-M-left])
    ;; (define-key input-decode-map "\e[1;7A" [C-M-up])
    ;; (define-key input-decode-map "\e[1;7B" [C-M-down])
    ;; (define-key input-decode-map "\e[13;5u" [(control return)])
    ))

(add-hook 'after-make-frame-functions 'jl/terminal-setup)

;; Full paths so I can use ffap to jump to them
(let ((config-files '("~/.emacs.d/user.d/packages.el"
		      "~/.emacs.d/user.d/core.el"
		      "~/.emacs.d/user.d/theme.el"
		      "~/.emacs.d/user.d/file-format.el"
		      "~/.emacs.d/user.d/bind-user.el"
		      "~/.emacs.d/user.d/bind-global-override.el"
		      "~/.emacs.d/user.d/local.el")))
  (dolist (filepath config-files)
    (jl/load-if-readable filepath)))

(defun vim ()
  (interactive)
  (load "~/.emacs.d/user.d/evil.el"))

;; Always load this file as an entry point into my config
(find-file load-file-name)

;; Use TCP for emacs daemon
(setq server-use-tcp t)
