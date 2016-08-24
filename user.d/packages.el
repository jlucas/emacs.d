;;;
;;; Packages
;;;

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package ibuffer
  :ensure t
  :bind (("C-x C-b" . ibuffer))
  :config (progn
            (setq ibuffer-show-empty-filter-groups nil)
            (autoload 'ibuffer "ibuffer" "List buffers." t)))

(use-package projectile
  :ensure t
  :init (progn
          (projectile-global-mode)
          (setq projectile-enable-caching t)
          (use-package ibuffer-projectile
            :ensure t
            :bind ("C-x C-b" . ibuffer)
            :init (progn
                    (add-hook 'ibuffer-hook
                              (lambda ()
                                (ibuffer-projectile-set-filter-groups)
                                (unless (eq ibuffer-sorting-mode 'alphabetic)
                                  (ibuffer-do-sort-by-alphabetic))))
                    (bind-keys :map ibuffer-mode-map
                               ("c" . clean-buffer-list)
                               ("n" . ibuffer-forward-filter-group)
                               ("p" . ibuffer-backward-filter-group))))))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (unbind-key "C-c C-q" aggressive-indent-mode-map))

(use-package isearch+
  :ensure
  :config (global-git-gutter+-mode))

;; Recipe for making a global minor mode that is not active in certain major modes
;; From: http://stackoverflow.com/a/6849467
;;   (define-global-minor-mode my-global-smartscan-mode global-smartscan-mode
;;     (lambda ()
;;       (when (not (memq major-mode
;;                        (list 'inferior-python-mode))))))
;;   (my-global-smartscan-mode t)

(use-package smartscan
  :ensure t
  :bind (("M-p" . smartscan-symbol-go-backward)
         ("M-n" . smartscan-symbol-go-forward)))

(use-package git-gutter+
  :ensure
  :config (global-git-gutter+-mode))

(use-package git-blame
  :ensure)

(use-package mo-git-blame
  :ensure)

(use-package sudo-edit
  :ensure t)

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
  :ensure t
  :init
  (setq multi-term-program "/bin/bash")
  :config
  (setq term-unbind-key-list nil)
  (setq term-bind-key-alist (list (cons "M-x" 'execute-extended-command)))
  :bind
  (("C-c T" . multi-term)
   ("C-c t" . multi-term-dedicated-toggle)
   :map term-mode-map
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-j" . term-char-mode)
   :map term-raw-map
   ("M-o" . other-window)
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-j" . term-line-mode)))

;; Conflicts with multi-term package
;; (use-package with-editor
;;   :ensure t
;;   :config (add-hook 'term-mode-hook 'with-editor-export-editor))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-c u" . undo-tree-visualize)))

(use-package expand-region
  :ensure
  :bind (("M-=" . er/expand-region)
	 ("M--" . er/contract-region)))

(use-package wrap-region
  :ensure
  :config (wrap-region-global-mode t))

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
            (slime-setup)
            ;; Fall back to using etags if slime doesn't know about the function
            (add-hook 'slime-edit-definition-hooks
                      #'(lambda (name unused) (slime-edit-definition-with-etags name))
                      t)))

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
  :ensure t
  :preface
  (defun jrl/magit-commit-verbose ()
    (interactive)
    (magit-commit (list (cons "-v" (magit-commit-arguments)))))
  :config (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-c g s" . magit-status)
         ("C-c g c" . jrl/magit-commit-verbose)
         ("C-c g h" . git-gutter+-stage-hunks)
         ("C-c g p" . magit-push-current-to-upstream)))

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
  :ensure t
  :init (recentf-mode t)
  :config (setq recentf-max-saved-items 100)
  :bind ("C-c f" . recentf-open-files))

(use-package edit-list
  :ensure t)

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init (progn
          (beacon-mode 1)
          (setq beacon-push-mark 35)
          (setq beacon-color "#87ffaf")))

;; Adapted from the article Move Through Edit Points. This works like
;; the mark, except it cycles through edit points. It takes you
;; through your undo history without actually undoing anything.
;;
;; “C-u 0 C-c ,” will give a description of changes made.
(use-package goto-chg
  :ensure t
  :bind (("C-c ," . goto-last-change)
         ("C-c ." . goto-last-change-reverse)))

;; See Magnars’ tutorial on Emacs Rocks.
(use-package restclient
  :ensure t)

(use-package dedicated
  :bind ("C-c D" . dedicated-mode))

(use-package key-chord
  :ensure t
  :init
  (setq key-chord-one-key-delay 0.30)
  (key-chord-define-global "ZZ" 'save-buffer)
  (key-chord-define-global "ZF" 'find-file-at-point)
  (key-chord-define-global "ZQ" 'server-edit)
  (key-chord-define-global "za" 'hs-toggle-hiding)
  (key-chord-define-global "zr" 'hs-show-all)
  (key-chord-define-global "zm" 'hs-hide-all)
  :config (key-chord-mode t))

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        `((,(openwith-make-extension-regexp '("pdf"))
           "xpdf" (file))
          (,(openwith-make-extension-regexp '("mp3" "mp4" "wav"))
           "mplayer"  (file))
          (,(openwith-make-extension-regexp '("mp4" "avi" "mkv" "ogg" "webm"
                                              "flv" "wmv" "mpg" "mpeg"))
           "mplayer" (file))))
  (openwith-mode t))

(use-package zoom-window
  :ensure t
  :init (setq zoom-window-mode-line-color "color-27")
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package rainbow-delimiters
  :ensure
  ;; add to most programming modes
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package org
  :ensure
  :config (progn
            (unbind-key "C-j" org-mode-map)
            (unbind-key "M-h" org-mode-map)
            (jl/load-if-readable "~/.emacs.d/user.d/org.el")))

;; http://orgmode.org/worg/org-contrib/org-collector.html
;; (use-package org-collector
;;   :ensure)

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init (global-hungry-delete-mode))

;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :ensure t
  :bind ("M-w" . easy-kill))

(use-package browse-kill-ring
  :ensure t
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(use-package yaml-mode
  :ensure)

;; Mediawiki syntax highlighting.
;; C-j is my join line bind.

(use-package mediawiki
  :ensure t
  :config (progn
            (unbind-key "C-j" mediawiki-mode-map)
            (add-to-list 'auto-mode-alist
                         '("itsalltext.*\\.txt$" . mediawiki))
            (add-to-list 'auto-mode-alist
                         '("\\.wiki\\'" . mediawiki))
            (add-to-list 'auto-mode-alist
                         '("en\\.wikipedia\\.org" . mediawiki))
            (setq mediawiki-site-alist
                  (append '(("comms" "http://comms-wiki/" nil nil "/Special:AllPages"))
                          mediawiki-site-alist))))

(use-package redmine
  :load-path "packages/emacs-redmine"
  :init (defun redmine-my-project ()
          (interactive)
          (setq redmine-program (concat user-emacs-directory
                                        "packages/emacs-redmine/redmine.py"))
          (setq redmine-project-name "pipe-comms-global")
          (setq redmine-login-key "1de2b422cd710882c5ce3556b47a215eed6c2bf5")
          (setq redmine-url "http://redmine-comms/")
          (redmine-show-sprints)))

(use-package python
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"))
  (defun ipython ()
    (interactive)
    (execute-extended-command 'run-python))
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
        python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
        python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n"
        python-shell-completion-module-string-code ""))

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
          (custom-set-variables '(elscreen-tab-display-kill-screen nil))))

(use-package winner
  :ensure t
  :config
  ;; Just use 'winner-undo and 'winner-redo
  (setq winner-dont-bind-my-keys t)
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

(use-package dired
  :preface
  (defun play-audio-jack ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (message "Opening %s..." file)
      (call-process "/usr/bin/mplayer" nil 0 nil "-ao" "jack" file)
      (message "Opening %s done" file)))
  :config
  (use-package dired-x)
  (use-package dired+)
  (bind-key "-" (lambda () (interactive) (find-alternate-file "..")) dired-mode-map))

(use-package crontab-mode
  :ensure t)

(use-package edit-server
  :ensure t
  :config
  ;; For use with:
  ;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
  (edit-server-start))

;; For commands:
;; doremi-all-faces-fg+
;; doremi-all-faces-bg+
(use-package doremi-frm
  :ensure t)

(use-package lua-mode
  :ensure t)

;; https://github.com/7max/log4cl
;; (ql:quickload :log4cl)
;; (ql:quickload :log4slime)
;; (log4slime:install)
;; log4slime is installed via Quicklisp.  It points to a file in ~/quicklisp so you don't have to change your emacs configuration when you update it.
(let ((log4slime "~/quicklisp/log4slime-setup.el"))
  (if (file-readable-p log4slime)
      (progn
        (load log4slime)
        (global-log4slime-mode 1))))

