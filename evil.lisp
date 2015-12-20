;;;;
;;;; evil-mode
;;;;
;;;; This must come last as it relies on seeing what other modes are in
;;;; your setup in order to apply itself correctly
;;;;

;;; Evil mode plugins
;evil-commentary    20150628.... unsigned              Comment stuff out. A port of vim-commentary.
;evil-jumper        20150628.... unsigned              Jump like vimmers do!
;evil-leader        20140606.543 unsigned              let there be <leader>
;evil-numbers       20140606.551 unsigned              increment/decrement numbers like in vim
;evil-org           20150513.... unsigned              evil keybindings for org-mode
;evil-surround      20150605.... unsigned              emulate surround.vim from Vim
;evil-terminal-cursor-changer  20150710.... unsigned              Change cursor shape by evil state on terminal.
;evil-visual-mar... 20150202.... unsigned              Display evil marks on buffer
;evil-visualstar    20150514.... unsigned              Starts a * or # search from the visual selection

(setq evil-toggle-key "C-M-z")
(require 'evil)

;; evil-jumper
;; https://github.com/bling/evil-jumper
(require 'evil-jumper)
(global-evil-jumper-mode)

;; evil-terminal-cursor-changer
;; https://github.com/7696122/evil-terminal-cursor-changer
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer))
;; If want change cursor shape type, add below line. This is evil's setting.
;; (setq evil-visual-state-cursor 'box)
;; (setq evil-insert-state-cursor 'bar)
;; (setq evil-emacs-state-cursor 'hbar)

;; evil-visual-mark-mode
;; https://github.com/roman/evil-visual-mark-mode
(require 'evil-visual-mark-mode)

;; evil-numbers
;; https://github.com/cofi/evil-numbers
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Make <C-]> do The Right Thing
;; https://emacs.stackexchange.com/questions/608/evil-map-keybindings-the-vim-way
(define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))

(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert
   state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key,
   ;; return [escape] so as Key Lookup will use it.
   ((or
     (evil-insert-state-p)
     (evil-normal-state-p)
     (evil-replace-state-p)
     (evil-visual-state-p))
    [escape])
   ;; This is the best way I could infer for now to have C-c work
   ;; during evil-read-key.  Note: As long as I return [escape] in
   ;; normal-state, I don't need this.  ((eq
   ;; overriding-terminal-local-map evil-read-key-map) (keyboard-quit)
   ;; (kbd ""))
   ;(t
   ; (kbd "C-g"))
   ))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;;;
;;; Works around the fact that Evil uses read-event directly when in
;;; operator state, which doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;;;
;;; Not sure what behavior this changes, but might as well set it,
;;; seeing the Elisp manual's documentation of it.
;;(set-quit-char "C-c") ; recommended by the article, but doesn't work
;;(set-quit-char ?\a) ; works, but isn't useful
;;;
;;; Set up some window switching binds
(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)

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

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)
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
"{" 'paredit-backward-barf-sexp)

Invoke evil
(evil-mode)
