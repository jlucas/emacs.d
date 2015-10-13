;; Set the following options in ~/.Xresources (or the deprecated
;; ~/.Xdefaults)::
;;
;; xterm*metaSendsEscape: true
;; xterm*eightBitInput: false
;; xterm*vt100.modifyOtherKeys: 1
;; xterm*vt100.formatOtherKeys: 1

;; Influential .emacs files
;; https://github.com/alexdantas/.emacs.d/blob/master/config/keybindings.el

;;;
;;; Misc settings
;;;

;; Pick up any elisp loosies from here
(add-to-list 'load-path "~/.emacs.d/vendor")

;; No splash screen messages
(setq inhibit-startup-message t)

;; Silence description of the *scratch* buffer
(setq initial-scratch-message "")

;; Prevent the cursor from blinking
;;(blink-cursor-mode 0)

;; Accept simply 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Preserve scratch buffer across sessions
(require 'persistent-scratch)
(persistent-scratch-setup-default)

;; Remove UI elements from GUI mode
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;;;
;;; File formats
;;;

;; Use tcl-mode for module files
(add-to-list 'magic-mode-alist '("#%Module" . tcl-mode))

;; Use mail-mode for files that contain the string "/mutt"
;; http://www.emacswiki.org/emacs/MuttInEmacs
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '(".muttrc" . muttrc-mode))
(add-to-list 'auto-mode-alist '("/.mutt/rc" . muttrc-mode))
;; Stay out of my user binds, muttrc-mode...
(eval-after-load "muttrc-mode"
  '(progn
     ;; Don't override my windmove-left bind
     (define-key muttrc-mode-map (kbd "C-c h") nil)
     ;; Don't override my split-window-below bind
     (define-key muttrc-mode-map (kbd "C-c s") nil)))

;;;
;;; Fonts
;;;

;; (set-frame-font "Envy Code R-16" nil t)
;; (set-frame-font "Terminus-11" nil t)
(set-frame-font "Monospace-9" nil t)
;; (set-face-attribute 'default nil :height 100)

;; Resizing fonts
;; C-x C-= to increase
;; C-x C-- to decrease

;;;
;;; Newline setup
;;;

;; Always try to indent on a new line
;; XXX: Is this why pasting into terminal freaks out sometimes?
(global-set-key (kbd "RET") 'newline-and-indent)

;;;
;;; User-reserved binds
;;;
;;; The "C-c [a-zA-Z]" space is reserved for users.
;;; http://stackoverflow.com/questions/1144424
;;;

;; Toggle line numbers on 'C-c l' (linum mode)
(global-set-key (kbd "C-c n") 'linum-mode)

;; Move around windows with vim-like hjkl binds
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;; Balance windows likw "C-w =" in vim
(global-set-key (kbd "C-c =") 'balance-windows)
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

;; zt, zb as in vim
;; Not really necessary...  C-l is pretty convenient.
(global-set-key (kbd "C-c t") (lambda () (interactive) (recenter 2)))
(global-set-key (kbd "C-c b") (lambda () (interactive) (recenter -3)))

;; H, M, L as in vim
(global-set-key (kbd "C-c H")(lambda () (interactive) (move-to-window-line-top-bottom 0)))
(global-set-key (kbd "C-c M") (lambda () (interactive) (move-to-window-line-top-bottom)))
(global-set-key (kbd "C-c L") (lambda () (interactive) (move-to-window-line-top-bottom -1)))

;; New GUI-mode frame (to be used when in terminal mode)
(global-set-key (kbd "C-c F")
                (lambda ()
                  (interactive)
                  (make-frame-on-display (getenv "DISPLAY"))))

;; Undo tree
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)

;; Join line as in vim
(global-set-key (kbd "C-c J") 'join-line)

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

;;;
;;; End user-reserved binds
;;;

;;;
;;; Global override binds
;;;

;; C-SPC is my tmux prefix!
;; You can get M-SPC #'just-one-space functionality with M-\
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-region) ; instead of default bind to C-w
(global-set-key (kbd "C-w") 'backward-kill-word) ; as in the shell, vim. etc.

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

;;;
;;; End global override binds
;;;

;;;
;;; X settings
;;;

;; Use PRIMARY selection in X
;; From: http://emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-primary t)

;; Include newline when killing a line
(setq kill-whole-line t)

;; Fullscreen in X with f11 key
;; From: http://emacswiki.org/emacs/FullScreen#toc22
(defun fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
(global-set-key [f11] 'fullscreen)

;;;
;;; magit
;;;
(setq magit-last-seen-setup-instructions "1.4.0") ; silence warnings

;;;
;;; ace-jump-mode
;;;

; https://github.com/winterTTr/ace-jump-mode
(autoload
 'ace-jump-mode
 "ace-jump-mode"
 "Emacs quick move minor mode"
 t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
 'ace-jump-mode-pop-mark
 "ace-jump-mode"
 "Ace jump back"
 t)
(eval-after-load "ace-jump-mode"
 '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x C-n") 'ace-jump-mode-pop-mark)

;; window-margin
;; https://github.com/aculich/window-margin.el.git
(add-to-list 'load-path "~/.emacs.d/vendor/window-margin")
(require 'window-margin)

;;;
;;; speedbar
;;;
(setq sr-speedbar-show-unknown-files t)

;;;
;;; sr-speedbar
;;;

;; http://www.emacswiki.org/emacs/SrSpeedbar
;; "SrSpeedbar is mode make SpeedBar show in Current Frame by SebastianRose."
(require 'sr-speedbar)

;;;
;;; undo-tree
;;;
(require 'undo-tree)
(global-undo-tree-mode 1)

;;;
;;; expand-region
;;;

;; https://github.com/magnars/expand-region.el
(require 'expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "M--") 'er/contract-region)

;;;
;;; wrap-region
;;;

;; Similar to vim-surround.  Wrap region with M-', M-", M-(, etc.
(require 'wrap-region)

;;;
;;; lisp-mode
;;;

;; See: http://stackoverflow.com/questions/18289329
;; and emacs docs for font-lock-function-name-face
(defun colorize-first-atom-in-list ()
  (font-lock-add-keywords nil
                          '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
                             1 'font-lock-function-name-face))))
(add-hook 'lisp-mode-hook 'colorize-first-atom-in-list)
;; (add-hook 'emacs-lisp-mode-hook 'colorize-first-atom-in-list)

;;;
;;; slime
;;;

;; Syntax highlighting in the SLIME REPL
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
          ;; From lisp-mode.el
          nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))
(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
       slime-repl-prompt-face
       rear-nonsticky
       (slime-repl-prompt read-only font-lock-face intangible)))))

;;;
;;; emacs-lisp-mode
;;;

;; http://stackoverflow.com/questions/18289329
;; Highlight all defined functions for elisp
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)
;; Also consider: http://ergoemacs.org/emacs/xah-elisp-mode.html

;;;
;;; dired
;;;

;; Don't open multiple windows when navigating subdirectories
;; From: http://ergoemacs.org/emacs/emacs_dired_tips.html
(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive) (find-alternate-file "..")))
(defun play-audio-jack ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "/usr/bin/mplayer" nil 0 nil "-ao" "jack" file)
    (message "Opening %s done" file)))
(define-key dired-mode-map (kbd "C-c C-c") 'play-audio-jack)

;;;
;;; ibuffer
;;;

(require 'ibuffer)
(require 'ibuffer-vc)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
(setq ibuffer-show-empty-filter-groups nil)

;; Auto-save path
(defconst my-temp-dir "~/tmp")
(setq backup-directory-alist
      `((".*" . ,my-temp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-temp-dir t)))

;;; eshell
; find a way to make this work
;(define-key eshell-mode-map (kbd "C-M-f") 'find-file-at-point)

;;; Automatically indent on newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

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

;;; imenu (has no default bind)
;; Can replace 'imenu with 'ido-goto-symbol, or some helm thing later.
;; https://www.masteringemacs.org/article/effective-editing-movement
(global-set-key (kbd "M-I") 'imenu)

;;; Mouse scrolling and pointing in terminal mode
(require 'xt-mouse)
(defun up-slightly () (interactive) (scroll-up 2))
(defun down-slightly () (interactive) (scroll-down 2))
(global-set-key (kbd "<mouse-4>") 'down-slightly)
(global-set-key (kbd "<mouse-5>") 'up-slightly)

;;;
;;; Paredit
;;;
;; http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
(add-hook 'lisp-mode-hook (lambda () (paredit-mode)))
                                        ;eval-after-load 'paredit
                                        ; ;; need a binding that works in the terminal
                                        ; '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
                                        ; '(define-key paredit-mode-map (kbd "M-(") 'paredit-backward-barf-sexp)
                                        ; '(define-key paredit-mode-map (kbd "C-cw") 'paredit-wrap-round))

;; http://unix.stackexchange.com/questions/79374
(defun my-terminal-config (&optional frame)
  ;; You need this in order to make the hook run on the correct frame.
  ;; This is important when running emacsclient in a terminal under an
  ;; emacs server that was started in a graphical environment.
  ;; http://stackoverflow.com/questions/7616761
  (select-frame frame)
 ;; non-nil when emacs is started graphically
  (message (format "window-system is %s" window-system))
  ;; nil when emacs is started graphically
  (message (format "display-graphic-p is %s" (display-graphic-p)))
  (unless (display-graphic-p)
    (message "running my-terminal-config...")
    (xterm-mouse-mode t)
    (define-key input-decode-map "\e[1;5A" [C-up])
    (define-key input-decode-map "\e[1;5B" [C-down])
    (define-key input-decode-map "\e[1;5C" [C-right])
    (define-key input-decode-map "\e[1;5D" [C-left])
    (define-key input-decode-map "\e[13;5u" [(control return)])))

(add-hook 'after-make-frame-functions 'my-terminal-config)

;(add-hook 'paredit-mode-hook
;	  '(lambda ()
;	     (local-set-key (kbd "M-)") 'paredit-forward-slurp-sexp)
;	     (local-set-key (kbd "M-(") 'paredit-backward-slurp-sexp)
;	     (local-set-key (kbd "C-c w") 'paredit-wrap-round)))

;;; auto-complete
(require 'auto-complete)
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; http://superuser.com/questions/208488/how-do-i-re-open-a-file-in-emacs
(global-auto-complete-mode t)

;; Opt out of auto-complete mode in minibuffer
;(defun auto-complete-mode-maybe ()
;  "No maybe for you. Only AC!"
;  (unless (minibufferp (current-buffer))
;    (auto-complete-mode 1)))

;;; I miss vim's . bind to repeat the last command
;;; This is "C-x z" by default, but that's a terrible bind
;(global-set-key (kbd "C-\.") 'repeat)

;; Code folding
;; Emacs users don't seem to place much stock in cold folding
;; See: http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; From: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jlucas-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'jlucas-toggle-selective-display)

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
;  (file-wildcards (concat user-emacs-directory "elpa/slime-2*//*.elc")))
;;(add-to-list 'load-path "~/.emacs.d/elpa/slime-20150221.645/contrib")
(setq slime-contribs '(slime-fancy))
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;;;
;;; SLIME
;;;

;; Use the same evil-mode keys to switch in/out of the repl window
;(require 'slime-autoloads)
;(add-hook 'slime-load-hook 'em-slime-load)
;(slime-setup '(slime-fancy))

;; When using evil-mode use my window navigation bindinds
(if (boundp 'evil-state)
    (progn
      (define-key slime-repl-mode-map (kbd "M-h") 'evil-window-left)
      (define-key slime-repl-mode-map (kbd "M-j") 'evil-window-down)
      (define-key slime-repl-mode-map (kbd "M-k") 'evil-window-up)
      (define-key slime-repl-mode-map (kbd "M-l") 'evil-window-right)))

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
;; http://stackoverflow.com/questions/69934
(setq tab-stop-list (number-sequence 4 200 4))

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

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-git-gutter")
(require 'git-gutter)

;; unbound
;; call (describe-unbound-keys 5) to list keys
;; http://emacswiki.org/emacs/unbound.el
(add-to-list 'load-path "~/.emacs.d/vendor/unbound")
(require 'unbound)

;;; shell scripts
;(setq-default sh-basic-offset 2)
;(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
;(load-theme 'fogus
;(load-theme 'dorsey)
;(load-theme 'graham t)
(load-theme 'xterm16 t)
;(load-theme 'deep-thought t)
;(load-theme 'hickey t)
;(load-theme 'granger t)
;(load-theme 'odersky t)
;(load-theme 'fogus t)
;(load-theme 'jlucas t)
;(load-theme 'tao-yin t)

;;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;;; Clojure support
(load "~/.emacs.d/vendor/clojure")

;;; hippie expand
;;; Don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

;;; ido
;(setq ido-use-filename-at-point nil)

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
             (define-key function-key-map "\e[1;3B" [M-down])
             (define-key function-key-map "\e[1;2D" [S-left])
             (define-key function-key-map "\e[1;2C" [S-right])
             (define-key function-key-map "\e[1;2A" [S-up])
             (define-key function-key-map "\e[1;2B" [S-down]))) 

;;;
;;; linum
;;; 
; Line numbers
(setq linum-format "%3d ")

;;;
;;; org-mode
;;;
;; Support embedding the following languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (dot . t)
   (sqlite . t)
   (lisp . t)))
;; Prompt for a comment and add a datestamp to DONE items.
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
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)
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
      '((sequence "TASK(t)"
                  "INPROGRESS(i)"
                  "SCHEDULED(s)"
                  "FEEDBACK(f)"
                  "ONHOLD(h)"
                  "BLOCKED(b)"
                  "DELEGATED(g)"
                  "|"
                  "DONE(d)"
                  "CANCELLED(c)")))
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
;(load "~/.emacs.d/vendor/org-secretary.el")
(setq org-sec-me "jlucas")

;; http://orgmode.org/worg/org-contrib/org-collector.html
(require 'org-collector)

; Add only a datestamp to DONE items
;(setq org-log-done 'time)

;;; Set vertical window character in the terminal
;;; http://stackoverflow.com/questions/18210631
(unless (window-system)
  (set-face-inverse-video-p 'vertical-border t)
  (set-face-background 'vertical-border (face-background 'default))
  ;; (set-display-table-slot standard-display-table
  ;;                         'vertical-border
  ;;                         (make-glyph-code ?┃))
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?\ )))

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
  ")" 'paredit-forward-slurp-sexp
  "(" 'paredit-backward-slurp-sexp
  "}" 'paredit-forward-barf-sexp
  "{" 'paredit-backward-barf-sexp)

;;; Invoke evil
;(evil-mode)

;; Always open open this file for the time being
(find-file load-file-name)

;; Start a server (emacs --daemon) if there isn't one running already
(server-start)

