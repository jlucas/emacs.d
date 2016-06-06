;;;
;;; Packages
;;;

(require 'use-package)

(use-package isearch+
    :ensure
    :config (global-git-gutter+-mode))

(use-package git-gutter+
    :ensure
    :config (global-git-gutter+-mode))

(use-package git-blame
	:ensure)

(use-package mo-git-blame
  :ensure)

(use-package sudo-edit
  :ensure
  :bind (("C-x C-r" . sudo-edit-current-file)))

(defun multi-term-set-cursor-according-to-mode ()
  "Change cursor type according to multi-term mode."
  (cond
   ((term-in-char-mode)
    (setq cursor-type 'hbar))
   (t
    (setq cursor-type 'bar))))

(defun term-toggle-line-char-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;; https://github.com/rlister/emacs.d/blob/master/lisp/multi-term-cfg.el
(use-package multi-term
  :commands multi-term
  :config (setq multi-term-program "/bin/bash")
  :bind (("C-c t" . multi-term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package with-editor
  :ensure t
  :config (add-hook 'term-mode-hook 'with-editor-export-editor))

(use-package undo-tree
  :ensure
  :config (global-undo-tree-mode 1))

(use-package expand-region
  :ensure
  :bind (("M-=" . er/expand-region)
	 ("M--" . er/contract-region)))

(use-package wrap-region
  :ensure
  :config (wrap-region-global-mode t))

(use-package ibuffer
  :ensure
  :bind (("C-x C-b" . ibuffer))
  :config (progn
	    (setq ibuffer-show-empty-filter-groups nil)
	    (autoload 'ibuffer "ibuffer" "List buffers." t)))

(use-package ibuffer-vc
  :ensure
  :config (add-hook 'ibuffer-hook
		      (lambda ()
			(ibuffer-vc-set-filter-groups-by-vc-root)
			(unless (eq ibuffer-sorting-mode 'alphabetic)
			  (ibuffer-do-sort-by-alphabetic)))))

(use-package zoom-window
  :ensure
  :bind (("C-x C-z" . zoom-window-zoom))
  :config (setq zoom-window-mode-line-color "color27"))

(use-package change-inner
  :ensure
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)))

(use-package markdown-mode
  :ensure
  :config (progn
	     (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
	     (add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
	     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))

(use-package slime
  :ensure
  :config (progn
	    (setq inferior-lisp-program "sbcl")
	    (require 'slime-autoloads)
	    (setq slime-contribs '(slime-fancy slime-banner))
	    (slime-setup)))

;; (use-package slime-autoloads
;;   :ensure
;;   :config (slime-setup '(slime-fancy)))

;; call (describe-unbound-keys 5) to list keys
;; http://emacswiki.org/emacs/unbound.el
(use-package unbound
  :ensure)

;; Select between multiple matches for a tag
(use-package etags-select
  :ensure
  :bind (("M-?" . etags-select-find-tag-at-point)
	 ("M-." . etags-select-find-tag)))

(use-package magit
  :ensure
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package multiple-cursors
  :ensure
  :bind (("C-c m" . mc/edit-lines)))
;; multiple-cursors key binding discussion
;; http://endlessparentheses.com/multiple-cursors-keybinds.html


(use-package rainbow-delimiters
  :ensure
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :init (progn
            (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
            (add-hook 'lisp-mode-hook 'enable-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
            (add-hook 'scheme-mode-hook 'enable-paredit-mode))
  :bind (("C-M-k" . kill-sexp)))		; This is actually the
										; standard bind for kill-sexp,
										; but I have it overridden in
										; other modes

;; By default, Emacs will only draw a rectangular selection out to the
;; shortest line.  This package has a function to make it behave like
;; vim and extend the rectangular selection to the longest line.
;; See: http://emacs.stackexchange.com/questions/3659
;;
;; TODO: Write a function that acts like pressing $ in visual block
;; mode (note, this is implemented in evil-mode).  This should be
;; triggered when the cursor is already at the end of a line with a
;; rectangular region active and the user presses C-e again.
(use-package rectangle-utils
  :ensure
  :bind ("C-x r e" . extend-rectangle-to-end))

;; Preserve scratch buffer across sessions
(use-package persistent-scratch
  :ensure
  :config (persistent-scratch-setup-default))

;; According to https://www.emacswiki.org/emacs/RecentFiles this
;; package has been built into Emacs since version 21, but it's nice
;; to keep the config together with the binding here.
(use-package recentf
  :ensure
  :config (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open-files))

;; ;; Multiple instances of term
;; (use-package multi-term
;;   :ensure
;;   :init (setq multi-term-program "/bin/bash")
;;   :config (lambda ()
;; 	    (setq term-unbind-key-list nil)
;; 	    (setq term-bind-key-alist (list (cons "M-x" 'execute-extended-command))))
;;   :bind (("C-c t" . multi-term)
;; 	 ("C-c T" . multi-term-dedicated-toggle)))

(use-package zoom-window
  :ensure
  :init (setq zoom-window-mode-line-color "color-27")
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package rainbow-delimiters
  :ensure
  ;; add to most programming modes
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package org
  :ensure
  :config (jl/load-if-readable "~/.emacs.d/user.d/org.el"))

(use-package hungry-delete
  :ensure
  :bind ("M-\\" . hungry-delete-forward))

(use-package yaml-mode
  :ensure)

(use-package latex-preview-pane
  :ensure)

(use-package hippie-exp
  :ensure
  :config (progn
            (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially)
            (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name))
  :bind (("C-M-/" . hippie-expand)
         ("C-M-_" . hippie-expand)))

(use-package elscreen
  :ensure
  :init (progn
	  (elscreen-start)
	  (set-face-attribute 'elscreen-tab-background-face nil
			      :inherit 'default :background nil)
	  (custom-set-variables '(elscreen-tab-display-kill-screen nil)))
  :bind (("M-p" . elscreen-previous) ; built-in: C-z C-p
	 ("M-n" . elscreen-next))) ; built-in: C-z C-n

;; Just use 'winner-undo and 'winner-redo
(setq winner-dont-bind-my-keys t)
(when (fboundp 'winner-mode) 
  (winner-mode 1))

;; http://orgmode.org/worg/org-contrib/org-collector.html
;; (use-package org-collector
;;   :ensure)

;; org
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "M-h"))))

;; dired-x for 'F' bind which visits all marked files (dired-x.el
;; ships with emacs)
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "-")
  (lambda () (interactive) (find-alternate-file "..")))
(defun play-audio-jack ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "/usr/bin/mplayer" nil 0 nil "-ao" "jack" file)
    (message "Opening %s done" file)))
(define-key dired-mode-map (kbd "C-c C-c") 'play-audio-jack)
