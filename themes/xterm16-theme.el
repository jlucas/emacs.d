
(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme xterm16 "A dark color theme based on xterm16.vim")

(let ((*normal*             "#b0b0b0")
      (*background*         "#1c1c1c")
      (*comments*           "#8787af")
      (*constant*           "#8700d7")
      (*current-line*       "#333")
      (*cursor-underscore*  "#FFFAAA")
      (*keywords*           "#0087af")
      (*line-number*        "#afaf87")
      (*method-declaration* "#a8a8a8")
      (*mode-line-fg*       "#c6c6c6")
      (*mode-line-bg*       "#444444")
      (*mode-inactive-fg*   "#8a8a8a")
      (*mode-inactive-bg*   "#303030")
      (*number*             "#87afaf")
      (*operators*          "#00af87")
      (*warning*            "#9d7a04")
      (*regexp*             "#9d7")
      (*string*             "#87afaf")
      (*cyan*               "#00afaf")
      (*greenblue*          "#005f5f")
      (*lightgreenblue*     "#87ffaf")
      (*paren*              "#00af87")
      (*paren-match*        "#87ffaf")
      (*paren-mismatch*     "#ff0000")
      (*bright-orange*      "#ff8700")
      (*light-brown*        "#af8700"))

  (custom-theme-set-faces
   'xterm16

   `(default ((t (:foreground ,*normal* :background ,*background*))))
   `(bold ((t (:bold t))))
   `(fringe ((t (:background ,*background* :foreground ,*number*))))
   `(linum ((t (:background ,*background* :foreground ,*line-number* :bold nil :underline nil))))
   `(button ((t (:foreground ,*keywords* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:background ,*mode-line-bg* :foreground ,*normal*)))) ;; info header
   `(highlight ((t (:background ,*current-line*))))
   `(highlight-face ((t (:background ,*current-line*))))
   `(hl-line ((t (:background ,*current-line*))))
   `(info-xref ((t (:foreground ,*keywords* :underline t))))
   `(region ((t (:background ,*greenblue* :foreground ,*lightgreenblue*))))
   `(underline ((nil (:underline t))))
   `(paren ((t (:foreground ,*paren*))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments*))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-doc-face ((t (:foreground ,*string*))))
   `(font-lock-doc-string-face ((t (:foreground ,*string*))))
   `(font-lock-func-face ((t (:foreground ,*keywords*))))
   `(font-lock-function-name-face ((t (:foreground ,*keywords*))))
   `(font-lock-keyword-face ((t (:foreground ,*keywords*))))
   `(font-lock-negation-char-face ((t (:foreground ,*warning*))))
   `(font-lock-number-face ((t (:foreground ,*number*))))
   `(font-lock-preprocessor-face ((t (:foreground ,*keywords*))))
   `(font-lock-reference-face ((t (:foreground ,*constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,*regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,*regexp*))))
   `(font-lock-string-face ((t (:foreground ,*string*))))
   `(font-lock-type-face ((t (:foreground ,*operators*))))
   `(font-lock-variable-name-face ((t (:foreground, *cyan*))))
   `(font-lock-warning-face ((t (:foreground ,*warning*))))
   ;; `(font-lock-add-keywords 'lisp-mode
   ;;                          )
   ;`(font-lock-add-keywords-face nil '(("[\(\)]+" 1 font-lock-constant-face t)))
   ;`(font-lock-add-keywords nil '(("[\(\)]+" 1 font-lock-warning-face t)))
   ;`(font-lock-add-keywords nil '(("\\([\{\}\\[\]\(\)]+\\)" 1 font-lock-warning-face t)))
   ;`(font-lock-add-keywords nil '(("\\([\{\}\\[\]\(\)]+\\)" 1 ,parenthesis t)))
   ;`(font-lock-prepend-face ((t (:foreground ,*paren-match*))))

   ;; GUI
   ;`(fringe ((t (:background, *background*))))
   ;`(linum ((t (:background, *line-number*))))
   `(minibuffer-prompt ((t (:foreground, *cyan*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *normal*))))
   ;; `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *mode-inactive-bg* :background, *mode-inactive-bg*)))) ;; between splits

   ;; search
   `(isearch ((t (:background, *regexp* :foreground, *lightgreenblue*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *lightgreenblue*))))

   ;; rainbow-delimiters
  `(rainbow-delimiters-depth-1-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground ,*paren*))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,*paren*))))

  ;; show-paren-mode
  `(show-paren-match-face ((t (:foreground ,*paren-match* :bold t))))
  `(show-paren-mismatch-face ((t (:foreground ,*paren-mismatch* :bold t))))

  ;; dired
  `(dired-directory ((t (:ground ,*keywords* :weight bold))))
  `(dired-symlink ((t (:foreground ,*paren-match* :weight bold))))
  ; `(dired-flagged ((t (:foreground ,red))))
  ; `(dired-header ((t (:foreground ,type :weight bold))))
  ; `(dired-ignored ((t (:inherit shadow))))
  ; `(dired-mark ((t (:foreground ,type :weight bold))))
  ; `(dired-marked ((t (:foreground ,violet :weight bold))))
  ; `(dired-perm-write ((t (:foreground ,base :underline t))))
  ; `(dired-warning ((t (:foreground ,war))))

  ;; org-mode
  `(org-todo ((t (:foreground ,*bright-orange* :weight bold))))
    
  ;; eshell
  ;`(eshell-ls-archive-face ((t (:bold t :foreground "medium purple" :weight bold))))
  ;`(eshell-ls-backup-face ((t (:foreground "dim gray"))))
  ;`(eshell-ls-clutter-face ((t (:foreground "dim gray"))))
  ;`(eshell-ls-directory-face ((t (:bold t :foreground "medium slate blue" :weight bold))))
  ;`(eshell-ls-executable-face ((t (:bold t :foreground "aquamarine" :weight bold))))
  ;`(eshell-ls-missing-face ((t (:foreground "black"))))
  ;`(eshell-ls-picture-face ((t (:foreground "violet"))))
  ;`(eshell-ls-product-face ((t (:foreground "light steel blue"))))
  ;`(eshell-ls-readonly-face ((t (:foreground "aquamarine"))))
  ;`(eshell-ls-special-face ((t (:foreground "gold"))))
  `(eshell-ls-symlink-face ((t (:foreground ,*paren-match*))))
  ;`(eshell-ls-unreadable-face ((t (:foreground "dim gray"))))
  `(eshell-prompt ((t (:foreground ,*keywords* :weight bold))))

  ;; git-gutter+
  `(git-gutter+-added ((t (:foreground ,*bright-orange* :background ,*mode-line-fg*))))
  `(git-gutter+-deleted ((t (:foreground ,*light-brown* :background ,*mode-line-fg*))))
  `(git-gutter+-modified ((t (:foreground ,*light-brown* :background ,*mode-line-fg*))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'xterm16)

;; Local Variables:
;; no-byte-compile: t
;; End:
