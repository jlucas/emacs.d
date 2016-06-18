;;;
;;; File formats
;;;

;;
;; tcl mode
;;

;; Use tcl-mode for module files
(add-to-list 'magic-mode-alist '("#%Module" . tcl-mode))

;;
;; mail-mode
;;

;; Use mail-mode for files that contain the string "/mutt"
;; http://www.emacswiki.org/emacs/MuttInEmacs
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;;
;; mutt-mode
;;

;; Use muttrc-mode for mutt configs
(add-to-list 'auto-mode-alist '(".muttrc" . muttrc-mode))
(add-to-list 'auto-mode-alist '("/.mutt/rc" . muttrc-mode))

;; Stay out of my user binds, muttrc-mode...
(eval-after-load "muttrc-mode"
  '(progn
     ;; Don't override my windmove-left bind
     (define-key muttrc-mode-map (kbd "C-c h") nil)
     ;; Don't override my split-window-below bind
     (define-key muttrc-mode-map (kbd "C-c s") nil)
     (setq indent-tabs-mode nil)))

;;
;; python
;;

;; Perhaps look in to https://github.com/kaz-yos/eval-in-repl

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(defun ipython ()
  (interactive)
  (execute-extended-command 'run-python))

