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

;; Remove UI elements from GUI mode
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Show empty lines and indicate and start/end of buffer
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)

;;;
;;; File formats
;;;

;; Use tcl-mode for module files
(add-to-list 'magic-mode-alist '("#%Module" . tcl-mode))

;; Use mail-mode for files that contain the string "/mutt"
;; http://www.emacswiki.org/emacs/MuttInEmacs
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
;; This was always getting set to 'indent-relative for some reason...
;; (add-hook 'mail-mode-hook (lambda ()
;;                             (setq indent-line-function 'insert-relative)))

;; Use muttrc-mode for mutt configs
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
;;
;; XXX: Is this why pasting into terminal freaks out sometimes?
;;
;; NOTE: Confirmed.  If you have RET bound to 'newline-and-indent and
;; 'electric-indent-mode active, you will get double indentation and
;; it's horrible
;; (global-set-key (kbd "RET") 'newline-and-indent)

;;;
;;; User-reserved binds
;;;
;;; The "C-c [a-zA-Z]" space is reserved for users.
;;; http://stackoverflow.com/questions/1144424
;;;


(global-set-key (kbd "C-c m") 'hs-toggle-hiding)
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
;; Move to previous/next buffer
(global-set-key (kbd "C-c b n") 'switch-to-next-buffer)
(global-set-key (kbd "C-c b p") 'switch-to-prev-buffer)

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
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g c") (lambda ()
                                  (interactive)
                                  (magit-commit (list (cons "-v" (magit-commit-arguments))))))
(global-set-key (kbd "C-c g h") 'git-gutter+-stage-hunks)
(global-set-key (kbd "C-c g p") 'magit-push-current-to-upstream)

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

;; Swap windows
;; From: http://www.emacswiki.org/emacs/TransposeWindows
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

;;;
;;; End user-reserved binds
;;;

;;;
;;; Global override binds
;;;

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
(global-set-key (kbd "C-w") 'backward-kill-word) ; as in the shell, vim. etc.
(global-set-key (kbd "M-%") 'replace-regexp) ; do i ever not want this?

(defun move-up-line ()
  "Move display up one line"
  (interactive)
  (scroll-down 1)
  (previous-line 1))
(global-set-key "\M-p" 'move-up-line)

(defun move-down-line ()
  "Move display down one line"
  (interactive)
  (scroll-up 1)
  (next-line 1))
(global-set-key "\M-n" 'move-down-line)

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

;;; I miss vim's . bind to repeat the last command
;;; This is "C-x z" by default, but that's a terrible bind
;(global-set-key (kbd "C-\.") 'repeat)

;;;
;;; End global override binds
;;;

;;;
;;; python.el
;;;

;; In python.el C-c C-c is bound to PYTHON-SHELL-SEND-BUFFER but this
;; is very rarely what I want.
(eval-after-load 'python
  '(define-key python-mode-map
     (kbd "C-c C-c")
     '(lambda ()
        (interactive)
        (unless mark-active
            (mark-paragraph))
        (python-shell-send-region (region-beginning) (region-end)))))

;;;
;;; bracketed-paste-mode
;;;

;; What is it?  https://cirw.in/blog/bracketed-paste
(require 'bracketed-paste)
(bracketed-paste-enable)

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
;;; git-gutter
;;;
(require 'git-gutter+)
(global-git-gutter+-mode)

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

;;;
;;; multiple-cursors
;;;

(require 'multiple-cursors)
(global-set-key (kbd "C-c I") 'mc/mark-next-like-this)
;; NOTE: In case of contiguious lines you can probably get away with
;; "C-x SPC", select a block, then "C-x r t" and type a prefix.

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

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))

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

;; FIXME: load-theme doesn't seem to like the theme name as a string
(defun my-dired-load-theme ()
  "In dired mode, load color theme at point"
  (interactive)
  (let* ((file (file-name-nondirectory (dired-get-filename nil t)))
        (theme-name (car (split-string file "-theme.el"))))
    (message "Applying theme %s..." file)
    (load-theme theme-name)))

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

;;; Automatically indent on newlines
;; (define-key global-map (kbd "RET") 'newline-and-indent)

;;; Preserve history across sessions
;;; http://stackoverflow.com/questions/1229142
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

;;; zoom-window
;;; https://github.com/syohex/emacs-zoom-window
(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
;; List colors with M-x list-colors-display
(setq zoom-window-mode-line-color "color-27")

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

;; ;;; auto-complete
;; (require 'auto-complete)
;; (ac-config-default)
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; ;; http://superuser.com/questions/208488/how-do-i-re-open-a-file-in-emacs
;; (global-auto-complete-mode t)

;; Opt out of auto-complete mode in minibuffer
;(defun auto-complete-mode-maybe ()
;  "No maybe for you. Only AC!"
;  (unless (minibufferp (current-buffer))
;    (auto-complete-mode 1)))

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

;;;
;;; slime
;;;

;; Use the same evil-mode keys to switch in/out of the repl window
;; (require 'slime-autoloads)
;; (add-hook 'slime-load-hook 'em-slime-load)
;; (slime-setup '(slime-fancy))

;; (setq slime-protocol-version 'ignore)
;; (mapc #'delete-file
;;  (file-wildcards (concat user-emacs-directory "elpa/slime-2*//*.elc")))
;; (add-to-list 'load-path "~/.emacs.d/elpa/slime-20150221.645/contrib")
;;(setq slime-contribs '(slime-repl))
;;(setq slime-contribs '(slime-fancy))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(setq swank:*globally-redirect-io* t)

;; When using evil-mode use my window navigation bindinds
(if (boundp 'evil-state)
    (progn
      (define-key slime-repl-mode-map (kbd "M-h") 'evil-window-left)
      (define-key slime-repl-mode-map (kbd "M-j") 'evil-window-down)
      (define-key slime-repl-mode-map (kbd "M-k") 'evil-window-up)
      (define-key slime-repl-mode-map (kbd "M-l") 'evil-window-right)))

;(load (expand-file-name "/nfs/home/jlucas/ash-home/quicklisp/slime-helper.el"))


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
;; ;; Make indentation commands use space only (never tab character)
;; (setq-default indent-tabs-mode nil) ; Emacs 23, 24 default to t
;; ;; Set default tab char's display width to four spaces
;; (setq-default tab-width 4) ; Emacs 23, 24 default to 8
;; ;; http://stackoverflow.com/questions/69934
(setq tab-stop-list (number-sequence 4 200 4))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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
;;(load-theme 'fogus
;;(load-theme 'dorsey)
;;(load-theme 'graham t)
(load-theme 'xterm16 t)
;;(load-theme 'deep-thought t)
;;(load-theme 'hickey t)
;;(load-theme 'granger t)
;;(load-theme 'odersky t)
;;(load-theme 'fogus t)
;;(load-theme 'jlucas t)
;;(load-theme 'tao-yin t)

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

;;;
;;; ffap (find-file-at-point)
;;;

;; From: http://stackoverflow.com/questions/259354/goto-file-in-emacs
;; Replace C-x C-f and others with ffap versions, ala vim's gf command.
(ffap-bindings)

;;;
;;; org-mode
;;;

(load "org.lisp")

;;;
;;; wanderlust
;;; 

(load "wanderlust.lisp")

;;;
;;; evil
;;;

(load "evil.lisp")

;; Always open open this file for the time being
(find-file load-file-name)

;; Preserve scratch buffer across sessions
;; Had some problems with this coming too early in user.el.
(require 'persistent-scratch)
(persistent-scratch-setup-default)

;; Start a server (emacs --daemon) if there isn't one running already
(server-start)

