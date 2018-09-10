;;;
;;; Global override binds
;;;

;; Unbind describe-copying
(global-unset-key (kbd "C-h C-c"))
;; Unbind describe-no-warranty
(global-unset-key (kbd "C-h C-w"))
;; Unbind view-emacs-news
(global-unset-key (kbd "C-h n"))
;; Unbind about-emacs
(global-unset-key (kbd "C-h C-a"))
;; Unbind view-external-packages
(global-unset-key (kbd "C-h C-e"))

;;; Rebindings of shadowed built-ins

;; Emulate vim's dt
;; https://stackoverflow.com/questions/27162540
;; http://www.emacswiki.org/emacs/ZapToCharUsage
(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Emacs defaults:
;; M-h bound to mark-paragraph (now rebound to C-M-h)
;; C-M-h bound to mark-defun (now unbound)
(global-set-key (kbd "M-h" ) 'windmove-left)
(global-set-key (kbd "C-M-h") 'mark-paragraph)
;; nxml-mode binds M-h to 'nxml-mark-paragraph
(add-hook 'nxml-mode-hook
          (lambda () (local-unset-key (kbd "M-h"))))

;; Make undo easier on the hands
;; M-u usually bound to 'upcase-word
(global-set-key (kbd "M-u") 'undo-tree-undo)

;; Emacs defaults:
;; M-j and C-M-j bound to indent-new-comment-line
(global-set-key (kbd "M-j" ) 'windmove-down)
(global-set-key (kbd "C-M-j" ) 'indent-new-comment-line)

;; Emacs defaults:
;; M-k bound to kill-sentence (now rebound to C-M-k)
;; C-M-k bound to kill-sexp (now unbound, but kill-sentence has a
;; similar effect under paredit))
(global-set-key (kbd "M-k" ) 'windmove-up)
(global-set-key (kbd "C-M-k" ) 'kill-sentence)

;; Emacs defaults:
;; M-l bound to downcase-word (now rebound to C-M-l)
;; C-M-l bound to reposition-window (now unbound)
(global-set-key (kbd "M-l" ) 'windmove-right)
(global-set-key (kbd "C-M-l" ) 'downcase-word)

;; C-x C-v gets bound to 'ffap-alternate-file when you invoke
;; 'ffap-bindings as I do earlier in my config
(global-set-key (kbd "C-x C-v") 'find-alternate-file)

;; Join like Vim
(global-set-key (kbd "C-j") (lambda ()
			      (interactive)
			      (forward-line)
			      (join-line)))
;; Paredit tries to bind C-j to 'paredit-newline
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "C-j") nil))

(defun replace-characters-in-line (str)
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil)
  (kill-ring-save (region-beginning) (region-end))
  (newline)
  (insert-char (string-to-char str) (length (car kill-ring))))

;; Markdown h1
(global-set-key (kbd "C-c 1")
                (lambda () (interactive) (replace-characters-in-line "=")))
;; Markdown h2
(global-set-key (kbd "C-c 2")
                (lambda () (interactive) (replace-characters-in-line "-")))

;; C-SPC is my tmux prefix!
;; You can get M-SPC #'just-one-space functionality with M-\
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-region) ; instead of default bind to C-w
                                              ; usually bound to 'edit-kbd-macro
(global-set-key (kbd "C-w") 'backward-kill-word) ; as in the shell, vim. etc.
(global-set-key (kbd "M-%") 'replace-regexp) ; do i ever not want this?

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c w") 'backward-kill-line) ; C-u in readline

(defun scroll-window-down (arg)
  "Move display up one line"
  (interactive "p")
  (scroll-down arg))
(global-set-key "\M-\S-p" 'scroll-window-down)

(defun scroll-window-up (arg)
  "Move display down one line"
  (interactive "p")
  (scroll-up arg))
(global-set-key "\M-\S-n" 'scroll-window-up)

(global-set-key (kbd "M-p") 'backward-paragraph) ; built-in M-{
(global-set-key (kbd "M-n") 'forward-paragraph) ; built-in M-}

;; Remove whitespace from point to first non-whitespace char
(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word.  From: http://emacswiki.org/emacs/DeletingWhitespace#toc18"
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))
;; C-\ is normally 'toggle-input-method
(global-set-key (kbd "C-\\") 'whack-whitespace)

;; ;; Bind to launch term mode
;; (global-set-key (kbd "C-c C-s") (lambda ()
;; 				  (interactive) (term "/bin/bash")))

;;; I miss vim's . bind to repeat the last command
;;; This is "C-x z" by default, but that's a terrible bind
;;;(global-set-key (kbd "C-\.") 'repeat)

(defun my-isearch-yank-word-or-char-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'word))
  (isearch-yank-word-or-char)
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'my-isearch-yank-word-or-char-from-beginning
                             'isearch-yank-word-or-char
                             isearch-mode-map))

(add-hook 'isearch-mode-hook
          (lambda ()
            "Activate my customized Isearch word yank command."
            (substitute-key-definition 'isearch-yank-word-or-char
                                       'my-isearch-yank-word-or-char-from-beginning
                                       isearch-mode-map)))
