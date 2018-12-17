;;;
;;; Packages
;;;

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; See: http://www.totherme.org/configs/gds.html
;; See, also: https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  (defun my/reb-query-replace-this-regxp (replace)
    "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.

Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
    (interactive "sReplace with: ")
    (if (eq major-mode 'reb-mode)
        (let ((reg (reb-read-regexp)))
          (select-window reb-target-window)
          (save-excursion
            (beginning-of-buffer)
            (query-replace-regexp reg replace)))
      (message "Not in a re-builder buffer!")))
  :bind (:map reb-mode-map ("C-c M-%" . my/reb-query-replace-this-regxp)))

;; atomic chrome
;; websockets server to launch an editor via web browser plugins like
;; GhostText and Atomic Chrome
;; https://github.com/alpha22jp/atomic-chrome/
(use-package atomic-chrome
  :init
  (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-buffer-open-style 'frame))

;; org-jira
(use-package org-jira
  :config
  (setq jiralib-url "https://framestore.atlassian.net"))

;; find-file-at-point, ala vim's gf command
(use-package ffap
  :config
  (ffap-bindings))

;; (use-package org-gcal
;;   :config
;;   (setq org-gcal-client-id "oauth 2.0 client ID"
;;         org-gcal-client-secret "client secret"
;;         org-gcal-file-alist '(("jesse.lucas@framestore.com" . "~/org/gcal.org"))))

;; Instead of ace-jump-mode
;; See: http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
(use-package avy
  :bind (("M-c" . avy-goto-word-1)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (autoload 'ibuffer "ibuffer" "List buffers." t))

;; Trying ibuffer-vc again to see if it's faster than
;; ibuffer-projectile
(use-package ibuffer-vc
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic))))
  :config (setq vc-handled-backends '(Git))) ; My life is just Git right now

;; (use-package projectile
;;   :init (progn
;;           (setq projectile-keymap-prefix (kbd "C-x p"))
;;           (projectile-global-mode)
;;           (setq projectile-enable-caching t)
;;           (use-package ibuffer-projectile
;;             :bind ("C-x C-b" . ibuffer)
;;             :init (progn
;;                     (add-hook 'ibuffer-hook
;;                               (lambda ()
;;                                 (ibuffer-projectile-set-filter-groups)
;;                                 (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                                   (ibuffer-do-sort-by-alphabetic))))
;;                     (bind-keys :map ibuffer-mode-map
;;                                ("c" . clean-buffer-list)
;;                                ("n" . ibuffer-forward-filter-group)
;;                                ("p" . ibuffer-backward-filter-group))))))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; (use-package aggressive-indent
;;   :diminish aggressive-indent-mode
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;   (unbind-key "C-c C-q" aggressive-indent-mode-map))

;; (use-package isearch+

;; Recipe for making a global minor mode that is not active in certain major modes
;; From: http://stackoverflow.com/a/6849467
;;   (define-global-minor-mode my-global-smartscan-mode global-smartscan-mode
;;     (lambda ()
;;       (when (not (memq major-mode
;;                        (list 'inferior-python-mode))))))
;;   (my-global-smartscan-mode t)

;; Tag hopping without ctags
(use-package smartscan
  :bind (("M-p" . smartscan-symbol-go-backward)
         ("M-n" . smartscan-symbol-go-forward)))

(use-package git-gutter+
  :config
  (global-git-gutter+-mode)
  (defalias 'ggm (lambda ()
                   (interactive)
                   (if git-gutter+-mode
                       (git-gutter+-mode -1)
                     (git-gutter+-mode 1)))))

(use-package mo-git-blame

(use-package sudo-edit

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
  :config
  (setq multi-term-program "/bin/bash")
  (setq comint-prompt-read-only t)
  (setq term-unbind-key-list nil)
  (setq term-bind-key-alist (list (cons "M-x" 'execute-extended-command)))
  :bind
  (:map term-mode-map
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("C-j" . term-char-mode)
   :map term-raw-map
   ("M-o" . other-window)
   ("C-j" . term-line-mode)))

;; Conflicts with multi-term package
;; (use-package with-editor
;;   :config (add-hook 'term-mode-hook 'with-editor-export-editor))

(use-package undo-tree
  :config (global-undo-tree-mode 1)
  :bind (("C-c u" . undo-tree-visualize)))

(use-package expand-region
  :bind (("M-=" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package wrap-region
  :config (wrap-region-global-mode t))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package jira-markup-mode

(use-package slime
  :load-path "elisp/slime"
  :init
  (setq inferior-lisp-program "sbcl")
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy slime-banner))
  :config
  ; Delay loading log4slime because it breaks slime-banner
  (add-hook 'slime-connected-hook
            (lambda ()
              "https://github.com/sharplispers/log4cl"
            (let ((log4slime-file "~/quicklisp/log4slime-setup.el"))
              (cond ((file-exists-p log4slime-file)
                     (load log4slime-file)
                     (global-log4slime-mode t))
                    (t
                     (message (format "Could not find log4slime file: %s"
                                      log4slime-file)))))))
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (sbcl-libtcod ("sbcl") :env ("LD_LIBRARY_PATH=/nfs/sw/libtcod/libtcod-1.6.4/lib"))))
  ;; Fall back to using etags if slime doesn't know about the function
  (add-hook 'slime-edit-definition-hooks
            #'(lambda (name unused) (slime-edit-definition-with-etags name))
            t)
  ;; http://lispblog.xach.com/post/157864421363/the-slime-selector
  :bind (("C-c p" . slime-selector)))

;; Select between multiple matches for a tag
;; (use-package etags-select
;;   :bind (("M-?" . etags-select-find-tag-at-point)
;;          ("M-." . etags-select-find-tag)))

(use-package magit
  :preface
  (defun jrl/magit-commit-verbose ()
    (interactive)
    (magit-commit (list (cons "-v" (magit-commit-arguments)))))
  :config (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("C-x g" . magit-status)
         ("C-c g s" . magit-status)
         ("C-c g c" . jrl/magit-commit-verbose)
         ("C-c g h" . git-gutter+-stage-hunks)
         ("C-c g p" . magit-push-current-to-upstream)))

(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)))
;; multiple-cursors key binding discussion
;; http://endlessparentheses.com/multiple-cursors-keybinds.html


(use-package rainbow-delimiters
  :ensure
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook (lambda ()
                                    (define-key slime-repl-mode-map
                                      (kbd "M-s") 'paredit-splice-sexp)))
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
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
  :config
  (persistent-scratch-setup-default)
  (when server-name
    (setq persistent-scratch-save-file
          (format "%s/.emacs.d/.persistent-scratch-%s"
                  (getenv "HOME")
                  server-name))))

;; According to https://www.emacswiki.org/emacs/RecentFiles this
;; package has been built into Emacs since version 21, but it's nice
;; to keep the config together with the binding here.
(use-package recentf
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode t)
  :bind
  ("C-c f" . recentf-open-files))

(use-package edit-list

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#87ffaf"))

;; Adapted from the article Move Through Edit Points. This works like
;; the mark, except it cycles through edit points. It takes you
;; through your undo history without actually undoing anything.
;;
;; “C-u 0 C-c ,” will give a description of changes made.
(use-package goto-chg
  :bind (("C-c ," . goto-last-change)
         ("C-c ." . goto-last-change-reverse)))

;; See Magnars’ tutorial on Emacs Rocks.
(use-package restclient

(use-package dedicated
  :bind ("C-c D" . dedicated-mode))

(use-package key-chord
  :config
  (setq key-chord-one-key-delay 0.30)
  (key-chord-define-global "ZZ" 'save-buffer)
  (key-chord-define-global "ZQ" 'server-edit)
  (key-chord-define-global "za" 'hs-toggle-hiding)
  (key-chord-define-global "zr" 'hs-show-all)
  (key-chord-define-global "zm" 'hs-hide-all)
  (key-chord-define-global "gf" 'find-file-at-point) ; ala vim
  (key-chord-mode t))

(use-package openwith
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
  :config (setq zoom-window-mode-line-color "color-27")
  :bind ("C-x C-z" . zoom-window-zoom))

(use-package rainbow-delimiters
  ;; add to most programming modes
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package org
  :config
  (unbind-key "C-j" org-mode-map)  ; Globally bound to my join-line function
  (unbind-key "M-h" org-mode-map)  ; Globally bound to 'windmove-left
  (jl/load-if-readable "~/.emacs.d/user.d/org.el")
  :bind ("C-c a" . org-agenda))

;; http://orgmode.org/worg/org-contrib/org-collector.html
;; (use-package org-collector
;;   :ensure)

(use-package hungry-delete
  :diminish hungry-delete-mode)

;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :bind ("M-w" . easy-kill))

(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))

(use-package yaml-mode
  :ensure)

;; Mediawiki syntax highlighting.
;; C-j is my join line bind.

(use-package mediawiki
  :config
  (unbind-key "C-j" mediawiki-mode-map)
  (add-to-list 'auto-mode-alist
               '("itsalltext.*\\.txt$" . mediawiki))
  (add-to-list 'auto-mode-alist
               '("\\.wiki\\'" . mediawiki))
  (add-to-list 'auto-mode-alist
               '("en\\.wikipedia\\.org" . mediawiki))
  (setq mediawiki-site-alist
        (append '(("comms" "http://comms-wiki/" nil nil "/Special:AllPages"))
                mediawiki-site-alist)))

(use-package python
  :config
  (catch 'found-python
    (mapc #'(lambda (str)
              (if (executable-find str)
                  (throw 'found-python (setq python-shell-interpreter str))))
          (list "ipython-local" "ipython-site" "ipython" "python")))
  (setq python-shell-interpreter-args "")
  (defun ipython (&optional args)
    (interactive)
    (let ((python-shell-interpreter-args args))
      (run-python nil t t)))
  (defun my-python-shell-send-line ()
    (interactive)
    (save-excursion
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)
      (python-shell-send-region (region-beginning) (region-end))
      (deactivate-mark)))
  (defun my-python-shell-send-smartly ()
    (interactive)
    (cond ((use-region-p)
           (python-shell-send-region (region-beginning) (region-end)))
          (t
           (save-excursion
             (mark-paragraph)
             (python-shell-send-region (region-beginning) (region-end))
             (deactivate-mark)))))
  :bind (:map python-mode-map
              ("C-c C-e" . my-python-shell-send-line)
              ("C-c C-c" . my-python-shell-send-smartly)))

(use-package latex-preview-pane

(use-package hippie-exp
  :config
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name)
  :bind (("C-M-/" . hippie-expand)
         ("C-M-_" . hippie-expand)))

(use-package vdiff
  :if (fboundp 'define-fringe-bitmap)
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(use-package elscreen
  :config
  (elscreen-start)
  (set-face-attribute 'elscreen-tab-background-face nil
                      :inherit 'default :background nil)
  (custom-set-variables '(elscreen-tab-display-kill-screen nil)))

(use-package winner
  :config
  ;; Just use 'winner-undo and 'winner-redo
  (setq winner-dont-bind-my-keys t)
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

(use-package eshell
  :commands eshell
  :config
  (require 'em-alias)
  ;; Informed by:
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient")
  (setenv "GIT_EDITOR" "emacsclient")
  (setenv "PAGER" "cat")
  (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
  (setq eshell-prompt-function
        (lambda ()
          "Prompt with Git branch"
          (let ((branch
                 (replace-regexp-in-string
                  "\n$" ""
                  (shell-command-to-string
                   "git symbolic-ref HEAD 2>/dev/null"))))
            (concat
             (abbreviate-file-name (eshell/pwd))
             (when (> (length branch) 0)
               (format " (%s)"
                       (replace-regexp-in-string
                        "refs/heads/" ""
                        (propertize branch 'face `(:foreground "orange")))))
             (if (eq (user-uid) 0) " # " " $ ")))))
  (mapcar (lambda (x)
            (add-to-list 'eshell-command-aliases-list x))
          '(("ll" "ls -l $*")
            ("lla" "ls -la $*")
            ("ff" "find-file $1")
            ("e" "find-file $1")
            ("d" "dired $1")
            ("up2" "cd ../..")
            ("up3" "cd ../../..")
            ("up4" "cd ../../../..")
            ("up5" "cd ../../../../..")
            ("up6" "cd ../../../../../..")
            ("up7" "cd ../../../../../../..")
            ("up8" "cd ../../../../../../../..")
            ("up9" "cd ../../../../../../../../..")))
  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.  From:
http://www.howardism.org/Technical/Emacs/eshell-fun.html"
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)))
  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))
  (defun jl/eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much) ; Why not? (eshell/exit)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ("C-d" . jl/eshell-quit-or-delete-char))))
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (mapcar (lambda (x)
						(add-to-list 'eshell-visual-commands x))
					  '("ssh"
						"tail"
						"tig"
						"irssi"
						"bitchx"
						"talk"
						"ytalk"
						"mutt"
						"nano"))))
  :bind
  ("C-c t" . eshell-here)
  ("C-c T" . eshell))

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
  (setq dired-dwim-target t)  ;; http://emacs.stackexchange.com/a/5604
  (bind-key "-" (lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "o"
            (lambda ()
              (interactive)
              (message (format "running xdg-open on: %s" (dired-get-file-for-visit)))
              (call-process "firefox" nil nil nil (dired-get-file-for-visit)))
            dired-mode-map))

;;; Built-in Emacs package
;;; Use for 'F' bind, to open multiple tagged files in splits
;;; https://www.reddit.com/r/emacs/comments/2lzssf/dired_vs_diredx_vs_dired_vs_diredaux/
(use-package dired-x)

;; (use-package crontab-mode

;; For commands:
;; doremi-all-faces-fg+
;; doremi-all-faces-bg+
;; (use-package doremi-frm

;; Highlight TODO, FIXME, etc.
(use-package fic-mode
  :config
  (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE" "XXX"))
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package hydra
  :config
  (global-set-key
   (kbd "C-c y")
   (defhydra hydra-move
     (:body-pre (next-line))
     "move"
     ("n" next-line)
     ("d" (lambda ()
            (interactive)
            (next-line 4)))
     ("p" previous-line)
     ("u" (lambda ()
            (interactive)
            (previous-line 4)))
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("[" backward-paragraph)
     ("]" forward-paragraph)
     ("v" scroll-up-command)
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)
     ("q" (lambda ()
            (interactive)
            (setq hydra-deactivate t))))))

(use-package lua-mode

(use-package jabber
  :config
  (setq jabber-account-list
        '(("jesse.lucas@framestore.com"
           (:network-server . "talk.google.com")
           (:connection-type . ssl)))))

;; (use-package helm
;;   :diminish helm-mode
;;   :init
;;   (progn
;;     (require 'helm-config)
;;     (setq helm-candidate-number-limit 100)
;;     ;; From https://gist.github.com/antifuchs/9238468
;;     (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;           helm-input-idle-delay 0.01  ; this actually updates things
;;                                         ; reeeelatively quickly.
;;           helm-yas-display-key-on-candidate t
;;           helm-quick-update t
;;           helm-M-x-requires-pattern nil
;;           helm-ff-skip-boring-files t)
;;     (helm-mode))
;;   :bind (("C-c h" . helm-mini)
;;          ("C-h a" . helm-apropos)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x b" . helm-buffers-list)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x)
;;          ("C-x c o" . helm-occur)
;;          ("C-x c s" . helm-swoop)
;;          ("C-x c y" . helm-yas-complete)
;;          ("C-x c Y" . helm-yas-create-snippet-on-region)
;;          ("C-x c b" . my/helm-do-grep-book-notes)
;;          ("C-x c SPC" . helm-all-mark-rings)))
;; (ido-mode -1)

