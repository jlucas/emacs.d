;;;
;;; evil
;;;

(message "loading evil config...")

(setq evil-toggle-key "C-M-z")

(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)

;; Must be loaded before evil-mode
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
   "RET" (lambda ()
           (interactive)
           (dired (file-name-directory (buffer-file-name))))
   "e" 'find-file
   "f" 'fill-paragraph
   "w" 'save-buffer
   "c" 'delete-window
   "s" 'evil-window-split
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
   "{" 'paredit-backward-barf-sexp))

(use-package evil
  :ensure t
  :init (evil-mode 1))

(use-package evil-visual-mark-mode
  :ensure t
  :config (evil-visual-mark-mode))

(use-package evil-numbers
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

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
(define-key evil-normal-state-map (kbd ",}")
  'paredit-backward-slurp-sexp)
(define-key evil-normal-state-map (kbd ",{")
  'paredit-backward-barf-sexp)
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
