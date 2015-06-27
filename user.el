;; This is where your customizations should live

;; No splash screen messages
(setq inhibit-startup-message t)

;; Auto-save path
(defconst my-temp-dir "~/tmp")
(setq backup-directory-alist
      `((".*" . ,my-temp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-temp-dir t)))

(setf this '(isn't what i'm talking about))

;;; Preserve history across sessions
;;; http://stackoverflow.com/questions/1229142
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;;; zoom-window
;;; https://github.com/syohex/emacs-zoom-window
(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
(setq zoom-window-mode-line-color "gray")

;;; change-inner
;;; https://github.com/magnars/change-inner.el
(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;;; Mouse scrolling and pointing in terminal mode
(require 'xt-mouse)
(xterm-mouse-mode t)
(require 'mouse)
(defun up-slightly () (interactive) (scroll-up 2))
(defun down-slightly () (interactive) (scroll-down 2))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

;; Poor man's surround-vim
;; http://stackoverflow.com/questions/2951797
;; Use "M-(" to surround selection with parens
(global-set-key (kbd "M-\"") 'insert-pair)
(global-set-key (kbd "M-'") 'insert-pair)

;; Paredit
;eval-after-load 'paredit
; ;; need a binding that works in the terminal
; '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
; '(define-key paredit-mode-map (kbd "M-(") 'paredit-backward-barf-sexp)
; '(define-key paredit-mode-map (kbd "C-cw") 'paredit-wrap-round))

;(eval-after-load "paredit"
;  '(define-key paredit-mode-map (kbd "M-)") nil))

(add-hook 'paredit-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "M-)") 'paredit-forward-slurp-sexp)
	     (local-set-key (kbd "M-(") 'paredit-backward-barf-sexp)
	     (local-set-key (kbd "C-c w") 'paredit-wrap-round)))

;;; Mimic vim's % key to move back and forth between matching parens
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
     vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "M-%") 'goto-match-paren)

;;; auto-complete
(require 'auto-complete)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(auto-complete-mode t)

;;; I miss vim's . bind to repeat the last command
;;(global-set-key (kdb "C-." 'repeat))

;; Code folding
;; Emacs users don't seem to place much stock in cold folding
;; See: http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; From: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jlucas-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'jlucas-toggle-selective-display)

;; C-SPC is my tmux prefix!
;; You can get M-SPC ('just-one-space) functionality by doing M-\.
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Turn on automatic bracket insertion by pairs.  New in Emacs 24.
(if (>= emacs-major-version 24)
    (electric-pair-mode 1))

;; Always highlight matched parens
(show-paren-mode 1)
(setq show-paren-delay 0)

; Markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;(setq slime-protocol-version 'ignore)
;(mapc #'delete-file
;  (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))
;;(add-to-list 'load-path "~/.emacs.d/elpa/slime-20150221.645/contrib")
;(setq slime-contribs '(slime-fancy))
;(require 'slime)
;(require 'slime-autoloads)
;(slime-setup '(slime-repl))

;;; SLIME
;(load (expand-file-name "/nfs/home/jlucas/ash-home/quicklisp/slime-helper.el"))
;;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-repl))
;(setq SWANK:*GLOBALLY-REDIRECT-IO* t)

;(setq inferior-lisp-program "sbcl")
;(require 'slime-autoloads)
;(add-to-list 'load-path "~/.emacs.d/elpa/slime-20150221.645/contrib")
;(slime-setup '(slime-repl
;               slime-asdf
;               slime-sprof
;               slime-compiler-notes-tree
;               slime-hyperdoc
;               slime-indentation
;               slime-media
;               slime-fancy))

;; flycheck
;(require 'flycheck)

;; env PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;;; Set up tabbing behavior
;; Make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil) ; Emacs 23, 24 default to t
;; Set default tab char's display width to four spaces
(setq-default tab-width 4) ; Emacs 23, 24 default to 8
;; Set current buffer's tab char's display width to four spaces
;(setq tab-width 4)

;; Silence description of the *scratch* buffer
(setq initial-scratch-message "")

;; No menus unless in graphical mode
(unless (window-system)
  (menu-bar-mode 0))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 20)))

;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;; shell scripts
;(setq-default sh-basic-offset 2)
;(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)
;(load-theme 'tomorrow-night-bright t)
;(load-theme 'charcoal-black t)

(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;(color-theme-late-night)
     ;(color-theme-black)
     ))

;;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;;; Clojure support
(load "~/.emacs.d/vendor/clojure")

;;; hippie expand
;;; Don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

;;; ido
(setq ido-use-filename-at-point nil)

;;;; Shen mode
;(add-to-list 'load-path "~/.emacs.d/vendor/shen-mode")
;(require 'shen-mode)
;(require 'inf-shen)
;(setq inferior-shen-program "/nfs/home/jlucas/ash-home/src/lisp/shen/shen-16/Platforms/SBCL/shen")
;(setq inferior-shen-buffer "inf-shen")

;(when (>= emacs-major-version 24)
;  (require 'package)
;  (package-initialize)
;  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

;;; theme
;(load-theme 'fogus
;(load-theme 'dorsey)
;(load-theme 'graham t)
;(load-theme 'jlucas t)
(load-theme 'xterm16 t)

(require 'toggle-window)
(setq window-min-height 0) ; AFAIK Emacs' internal minimum is 1

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; add to most programming modes

;;;
;;; windmove
;;;
(windmove-default-keybindings 'meta)
;; These binds work in a terminal outside of tmux.
(add-hook 'term-setup-hook
          '(lambda ()
             (define-key function-key-map "\e[1;3D" [M-left])
             (define-key function-key-map "\e[1;3C" [M-right])
             (define-key function-key-map "\e[1;3A" [M-up])
             (define-key function-key-map "\e[1;3B" [M-down])))

;;;
;;; linum
;;; 
; Line numbers
(setq linum-format "%3d ")

;;;
;;; org-mode
;;;
;; Prompt for a comment and add a datestamp to DONE items.
;; Other possible value is 'time, which will just add a datestamp.
(setq org-log-done 'note)
;; http://orgmode.org/manual/Clean-view.html
;; If you decide you don't like this, you can enable it on specific
;; org files by adding the text "#+STARTUP: indent" somewhere in the
;; file
(setq org-startup-indented t)
;; Agenda files
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
;; From: http://orgmode.org/worg/org-faq.html#orgheadline41
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; From: http://orgmode.org/manual/Workflow-states.html
;(setq org-todo-keywords
;      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))
                           ("~/src/lispmud/lispmud.org" . (:maxlevel . 6))))
;; From: http://orgmode.org/manual/Internal-archiving.html#Internal-archiving
;; Show archived items when cycling with S-TAB
;; FIXME: Doesn't work in console emacs
;(setq org-columns-skip-archived-trees nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
        (sequence "TASK(f)" "|" "DONE(d)")
        (sequence "MAYBE(m)" "|" "CANCELLED(c)")))
(setq org-tags-exclude-from-inheritance '("prj")
      org-stuck-projects '("+prj/-MAYBE-DONE"
                           ("TODO" "TASK") ()))
(setq org-agenda-custom-commands
      '(("h" "Work todos" tags-todo
         "-personal-doat={.+}-dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled t)))
        ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("A" "Work todos with doat or dowith" tags-todo
         "-personal+doat={.+}|dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("j" "TODO dowith and TASK with"
         ((org-sec-with-view "TODO dowith")
          (org-sec-where-view "TODO doat")
          (org-sec-assigned-with-view "TASK with")
          (org-sec-stuck-with-view "STUCK with")))
        ("J" "Interactive TODO dowith and TASK with"
         ((org-sec-who-view "TODO dowith")))))
(eval-after-load 'org-secretary
  '(define-key org-mode-map (kbd "C-c w") 'org-sec-set-with))
(eval-after-load 'org-secretary
  '(define-key org-mode-map (kbd "C-c W") 'org-sec-set-where))
(load "~/.emacs.d/vendor/org-secretary.el")
(setq org-sec-me "jlucas")

; Add only a datestamp to DONE items
;(setq org-log-done 'time)

;;;;
;;;; evil-mode
;;;;
;;;; This must come last as it relies on seeing what other modes are in
;;;; your setup in order to apply itself correctly
;;;;
;(setq evil-toggle-key "C-M-z")
;(require 'evil)
;(defun my-esc (prompt)
;  "Functionality for escaping generally.  Includes exiting Evil insert
;   state and C-g binding. "
;  (cond
;   ;; If we're in one of the Evil states that defines [escape] key,
;   ;; return [escape] so as Key Lookup will use it.
;   ((or
;     (evil-insert-state-p)
;     (evil-normal-state-p)
;     (evil-replace-state-p)
;     (evil-visual-state-p))
;    [escape])
;   ;; This is the best way I could infer for now to have C-c work
;   ;; during evil-read-key.  Note: As long as I return [escape] in
;   ;; normal-state, I don't need this.  ((eq
;   ;; overriding-terminal-local-map evil-read-key-map) (keyboard-quit)
;   ;; (kbd ""))
;   ;(t
;   ; (kbd "C-g"))
;   ))
;(define-key key-translation-map (kbd "C-c") 'my-esc)
;;;;
;;;; Works around the fact that Evil uses read-event directly when in
;;;; operator state, which doesn't use the key-translation-map.
;(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;;;;
;;;; Not sure what behavior this changes, but might as well set it,
;;;; seeing the Elisp manual's documentation of it.
;;;(set-quit-char "C-c") ; recommended by the article, but doesn't work
;;;(set-quit-char ?\a) ; works, but isn't useful
;;;;
;;;; Set up some window switching binds
;;(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;;(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;;(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;;(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
;
;; Miss my mapleader
;(require 'evil-leader)
;(global-evil-leader-mode)
;(evil-leader/set-leader ",")
;(evil-leader/set-key
;  "n" 'linum-mode
;  "z" 'delete-other-windows
;  "RET" 'dired)
;
;;;; Invoke evil
;(evil-mode)

