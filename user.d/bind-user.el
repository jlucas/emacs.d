;;;
;;; User-reserved binds
;;;
;;; The "C-c [a-zA-Z]" space is reserved for users.
;;; http://stackoverflow.com/questions/1144424
;;;

;; https://www.emacswiki.org/emacs/HideShow
;; Don't forget about M-x occur
;;(global-set-key (kbd "C-c m") 'hs-toggle-hiding)
(global-set-key (kbd "C-c z") 'hs-hide-all)
(global-set-key (kbd "C-c Z") 'hs-show-all)

;; Toggle line numbers on 'C-c l' (linum mode)
(global-set-key (kbd "C-c n") 'linum-mode)

;; Move around windows with vim-like hjkl binds
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Balance windows likw "C-w =" in vim
(global-set-key (kbd "C-c =") (lambda ()
                                (interactive)
                                (message "The command is (balance-windows) and the default emacs bind for this is \"C-x +\".  Use that instead.")))
;; Vertical split
(global-set-key (kbd "C-c v") (lambda ()
                                (interactive)
                                (split-window-right)
                                (windmove-right)))
;; Horizontal split
(global-set-key (kbd "C-c s") (lambda ()
                                (interactive)
                                (split-window-below)
                                (windmove-down)))

;; Close window
(global-set-key (kbd "C-c c") 'delete-window)

(require 'xt-mouse)
(xterm-mouse-mode t)
(defun up-slightly () (interactive) (scroll-up 2))
(defun down-slightly () (interactive) (scroll-down 2))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

;; Switch to previous/next frame
(global-set-key (kbd "C-c N") (lambda ()
                                 (interactive)
                                 (select-frame-set-input-focus
                                  (next-frame)
                                  (message "Switched to frame: %s"
                                           (cdr (assoc 'name (frame-parameters)))))))
(global-set-key (kbd "C-c P") (lambda ()
                                 (interactive)
                                 (select-frame-set-input-focus
                                  (previous-frame)
                                  (message "Switched to frame: %s"
                                           (cdr (assoc 'name (frame-parameters)))))))

;; H, M, L as in vim
(global-set-key (kbd "C-c H")(lambda () (interactive) (move-to-window-line-top-bottom 0)))
(global-set-key (kbd "C-c M") (lambda () (interactive) (move-to-window-line-top-bottom)))
(global-set-key (kbd "C-c L") (lambda () (interactive) (move-to-window-line-top-bottom -1)))

;; New GUI-mode frame (to be used when in terminal mode)
(global-set-key (kbd "C-c F")
                (lambda ()
                  (interactive)
                  (make-frame-on-display (getenv "DISPLAY"))))

;; Join line as in vim
(global-set-key (kbd "C-j") (lambda ()
                              (interactive)
                              (forward-line)
                              (join-line)))

;; Just type the char you want to align your text to
(global-set-key (kbd "C-c a") 'align-regexp)

;; Make this buffer the least likely candidate for C-x b
(global-set-key (kbd "C-c r") 'bury-buffer)

;; Easy block indent
(global-set-key (kbd "C-c >")
                (lambda ()
                  (interactive)
                  (indent-rigidly (region-beginning) (region-end) 4)))
(global-set-key (kbd "C-c <")
                (lambda ()
                  (interactive)
                  (indent-rigidly (region-beginning) (region-end) -4)))

;;; % to bounce between balanced parens as in vim
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
     vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-c %") 'goto-match-paren)

;; Similar to C-o and C-i in vim
(global-set-key (kbd "C-c o") 'previous-buffer)
(global-set-key (kbd "C-c i") 'next-buffer)

;; Invoke dired mode quickly
(global-set-key (kbd "C-c d") (lambda ()
                                (interactive)
                                (dired default-directory)))

;; Code folding
;; Emacs users don't seem to place much stock in cold folding
;; See: http://stackoverflow.com/questions/1085170
;; From: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(global-set-key [f1] (lambda ()
		       (interactive)
		       (set-selective-display (if selective-display nil 1))))

;; Swap windows
;; From: http://www.emacswiki.org/emacs/TransposeWindows
(setq swapping-window nil)
(setq swapping-buffer nil)
(defun swap-buffers-in-windows ()
   "Swap buffers between two windows"
   (interactive)
   (if (and swapping-window
            swapping-buffer)
       (let ((this-buffer (current-buffer))
             (this-window (selected-window)))
         (if (and (window-live-p swapping-window)
                  (buffer-live-p swapping-buffer))
             (progn (switch-to-buffer swapping-buffer)
                    (select-window swapping-window)
                    (switch-to-buffer this-buffer)
                    (select-window this-window)
                    (message "Swapped buffers."))
           (message "Old buffer/window killed.  Aborting."))
         (setq swapping-buffer nil)
         (setq swapping-window nil))
     (progn
       (setq swapping-buffer (current-buffer))
       (setq swapping-window (selected-window))
       (message "Buffer and window marked for swapping."))))
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)
