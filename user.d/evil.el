;;;
;;; evil
;;;

(message "Loading evil config...")

(setq evil-toggle-key "C-M-z")

;; Must be loaded before evil-mode
(use-package evil-leader
  :ensure
  :init (progn
	    (global-evil-leader-mode 1)
	    (evil-leader/set-leader "<SPC>")
	    (evil-leader/set-key
	     "RET" (lambda ()
	    	    (interactive)
	    	    (dired (file-name-directory (buffer-file-name))))
	     "e" 'find-file
	     "f" 'fill-paragraph
	     "w" 'save-buffer
	     "W" 'paredit-wrap-round
	     "c" 'delete-window
	     "s" 'evil-window-split
	     "S" 'paredit-splice-sexp
	     "v" 'evil-window-vsplit
	     "b" 'switch-to-buffer
	     "B" 'ibuffer
	     "n" 'linum-mode
	     "q" 'save-buffers-kill-terminal
	     "Q" 'kill-emacs
	     "u" 'undo-tree-visualize
	     "z" 'delete-other-windows
	     ">" 'paredit-forward-slurp-sexp
	     "<" 'paredit-forward-barf-sexp
	     "}" 'paredit-backward-slurp-sexp
	     "{" 'paredit-backward-barf-sexp)))

(use-package evil
  :ensure
  :init (evil-mode))

(use-package evil-jumper
  :ensure
  :config (global-evil-jumper-mode))

(use-package evil-visual-mark-mode
  :ensure
  :config (evil-visual-mark-mode))

(use-package evil-numbers
  :ensure
  :config (progn
	    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
	    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)))

(use-package evil-commentary
  :ensure
  :config (evil-commentary-mode))

(use-package evil-surround
  :ensure
  :config (global-evil-surround-mode 1))

;; (use-package evil-terminal-cursor-change
;;   :ensure)


;; Make "C-]" do the right thing
;; https://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way
(define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))

;; expand-region binds matching vim-expand-region
(define-key evil-visual-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "-") 'er/contract-region)

;; Move down a split and maximize it
(define-key evil-normal-state-map (kbd "C-j")
  (lambda ()
    (interactive)
    (evil-window-down 1)
    (evil-window-set-height (frame-height))))

;; Move up a split and maximize it
(define-key evil-normal-state-map (kbd "C-k")
  (lambda ()
    (interactive)
    (evil-window-up 1)
    (evil-window-set-height (frame-height))))

;; Emulate paredit.vim
(define-key evil-normal-state-map (kbd ",>")
  'paredit-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd ",<")
  'paredit-forward-barf-sexp)
(define-key evil-normal-state-map (kbd ",S")
  'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd ",W")
  'paredit-wrap-round)

(if (boundp 'evil-state)
    (progn
      (define-key slime-repl-mode-map (kbd "M-h") 'evil-window-left)
      (define-key slime-repl-mode-map (kbd "M-j") 'evil-window-down)
      (define-key slime-repl-mode-map (kbd "M-k") 'evil-window-up)
      (define-key slime-repl-mode-map (kbd "M-l") 'evil-window-right)))

(message "Evil config loaded.")
