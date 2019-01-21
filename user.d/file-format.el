;;;
;;; File formats
;;;

;;;
;;; hide-show mode
;;;

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)

;;
;; c++ mode
;;

;; Stick with 'mark-paragraph instead of 'c-mark-function
(unbind-key "C-M-h" c++-mode-map)

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
(add-hook 'mail-mode-hook 'flyspell-mode)

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
;; inputrc
;;

;; Why doesn't Emacs Just Work?
(add-to-list 'auto-mode-alist '("inputrc" . conf-mode))

;;
;; strace
;;

;; Set up as a git submodule
;; https://github.com/pkmoore/strace-mode
(let ((strace "~/.emacs.d/elisp/strace-mode/strace-mode.el"))
  (if (file-readable-p strace)
      (load strace)))

;;;
;;; Maya Embedded Language (MEL)
;;;

(add-to-list 'auto-mode-alist '("\\.mel$" . mel-mode))
(autoload 'mel-mode "mel-mode" nil t)

