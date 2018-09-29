
(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme graham-crackers "Subdued gray, orange, and light blue theme")

(let ((*normal*             "#eeeeee")  ; 255 gray93
      (*background*         "#1c1c1c")  ; 234 grey11
      ;(*comments*           "#5f5f00")  ; 58 orange4
      ;(*comments*           "#87875f")  ; 101 wheat4
      (*comments*           "#875f5f")  ; 95 lightpink4
      ;(*comments*           "#875f00")  ; 94 orange4
      (*constant*           "#ff8700")  ; 208 darkorange
      (*variable*           "#afafd7")  ; 146 lightsteelblue3
      (*operators*          "#ff8700")  ; 208 darkorange
      (*current-line*       "#303030")  ; 236 grey19
      (*cursor*             "#b2b2b2")  ; 249 grey70
      (*cursor-underscore*  "#ffafaf")  ; 217 lightpink1
      (*darkpurple*         "#8700af")  ; 91 darkmagenta
      (*keywords*           "#ffaf00")  ; 214 orange1
      (*line-number*        "#afaf87")  ; 144 navajowhite3
      (*method-declaration* "#ffd7af")  ; 223 navajowhite1
      (*gray236*            "#303030")  ; 236 grey19
      (*gray238*            "#444444")  ; 238 grey27
      (*gray245*            "#8a8a8a")  ; 245 grey54
      (*gray251*            "#c6c6c6")  ; 251 grey78
      (*number*             "#ff5f00")  ; 202 orangered1
      (*warning*            "#ff5f5f")  ; 203 indianred1
      (*error*              "#ff5f5f")  ; 203 indianred1
      (*regexp*             "#90d070")  ; ???
      (*string*             "#afafaf")  ; 145 grey69
      (*bluegreen*          "#00af87")  ; 36 darkcyan
      (*cyan*               "#00afaf")  ; 37 lightseagreen
      (*greenblue*          "#005f5f")  ; 23 deepskyblue4
      (*lightgreenblue*     "#87ffaf")  ; 121 palegreen1
      (*paren*              "#b2b2b2")  ; 249 grey70
      (*paren-alt*          "#afafd7")  ; 146 lightsteelblue3
      (*paren-match*        "#ffd700")  ; 220 gold1
      (*paren-mismatch*     "#d70000")  ; 160 red3
      (*match*              "#5f87ff")  ; 69 cornflowerblue
      (*match-bg*           "#87ffaf")  ; 121 palegreen1
      (*match-fg*           "#005f5f")  ; 23 deepskyblue4
      (*region*             "#303030")  ; 236 grey19
      (*bright-orange*      "#ff8700")  ; 220 gold1
      (*trailing-space*     "#d70000")  ; 160 red3
      (*mode-line-bg*       "#eeeeee")  ; 255 grey93
      (*mode-line-fg*       "#303030")  ; 236 grey19
      (*mode-inactive-bg*   "#303030")  ; 236 grey19
      (*mode-inactive-fg*   "#eeeeee")  ; 255 grey93
      (*light-brown*        "#af8700")  ; 136 darkgoldenrod
      (*yellow*             "#afaf87")  ; 144 navajowhite3
      )

  (custom-theme-set-faces
   'graham-crackers

   `(default ((t (:foreground ,*normal* :background ,*background*))))
   `(bold ((t (:bold t))))
   `(fringe ((t (:background ,*background* :foreground ,*number*))))
   `(linum ((t (:background ,*background* :foreground ,*line-number* :bold nil :underline nil))))
   `(button ((t (:foreground ,*keywords* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:background ,*gray238* :foreground ,*normal*)))) ;; info header
   `(highlight ((t (:background ,*current-line*))))
   `(highlight-face ((t (:background ,*current-line*))))
   `(hl-line ((t (:background ,*current-line*))))
   `(info-xref ((t (:foreground ,*keywords* :underline t))))
   `(region ((t (:background ,*region*))))
   `(underline ((nil (:underline t))))
   `(paren ((t (:foreground ,*paren*))))
   `(trailing-whitespace
     ((t (:foreground ,*trailing-space* :background ,*background* :underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments*))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-doc-face ((t (:foreground ,*string*))))
   `(font-lock-doc-string-face ((t (:foreground ,*string*))))
   `(font-lock-func-face ((t (:foreground ,*keywords*))))
   `(font-lock-function-name-face ((t (:foreground ,*method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground ,*keywords*))))
   `(font-lock-negation-char-face ((t (:foreground ,*warning*))))
   `(font-lock-number-face ((t (:foreground ,*number*))))
   `(font-lock-preprocessor-face ((t (:foreground ,*keywords*))))
   `(font-lock-reference-face ((t (:foreground ,*constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,*regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,*regexp*))))
   `(font-lock-string-face ((t (:foreground ,*string*))))
   `(font-lock-type-face ((t (:foreground ,*operators*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
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
   `(minibuffer-prompt ((t (:foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *cursor*))))
   `(vertical-border ((t (:foreground, *gray236* :background, *gray236*)))) ;; between splits

   ;; compilation, grep
   `(match ((t (:background ,*match-fg* :foreground ,*match-bg*))))
   `(compilation-info ((t (:foreground ,*method-declaration*))))
   `(compilation-line-number ((t (:foreground ,*number*))))
   `(compilation-error ((t (:foreground ,*error*))))

   ;; search
   `(isearch ((t (:background ,*match-bg* :foreground ,*match-fg*))))
   `(isearch-fail ((t (:background ,*warning*))))
   `(lazy-highlight ((t (:background ,*match-fg* :foreground ,*match-bg*))))

   ;; show-paren-mode
   `(show-paren-match ((t (:foreground ,*paren-match* :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,*paren-mismatch* :weight bold))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,*paren*))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,*paren-alt*))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,*paren*))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,*paren-alt*))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,*paren*))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,*paren-alt*))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,*paren*))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,*paren-alt*))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,*paren*))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,*paren*))))

   ;; ;; dired
   ;; `(dired-directory ((t (:ground ,*keywords* :weight bold))))
   ;; `(dired-symlink ((t (:foreground ,*paren-match* :weight bold))))
   ;;                                      ; `(dired-flagged ((t (:foreground ,red))))
   ;;                                      ; `(dired-header ((t (:foreground ,type :weight bold))))
   ;;                                      ; `(dired-ignored ((t (:inherit shadow))))
   ;;                                      ; `(dired-mark ((t (:foreground ,type :weight bold))))
   ;;                                      ; `(dired-marked ((t (:foreground ,violet :weight bold))))
   ;;                                      ; `(dired-perm-write ((t (:foreground ,base :underline t))))
   ;;                                      ; `(dired-warning ((t (:foreground ,war))))

   ;; org-mode
   `(org-todo ((t (:foreground ,*bright-orange* :weight bold))))
   `(org-date ((t (:foreground ,*bluegreen* :weight bold))))
   `(org-level-4 ((t (:foreground ,*constant*))))

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
   `(git-gutter+-added ((t (:foreground ,*bright-orange* :background ,*gray238*))))
   `(git-gutter+-deleted ((t (:foreground ,*light-brown* :background ,*gray238*))))
   `(git-gutter+-modified ((t (:foreground ,*light-brown* :background ,*gray238*))))

   ;; elscreen
   `(elscreen-tab-background-face ((t (:background ,*background*))))
   `(elscreen-tab-control-face ((t (:foreground ,*mode-line-fg* :background ,*mode-line-bg*))))
   `(elscreen-tab-current-screen-face ((t (:foreground ,*mode-line-fg* :background ,*mode-line-bg*))))
   `(elscreen-tab-other-screen-face ((t (:foreground ,*mode-inactive-fg* :background ,*mode-inactive-bg*))))

   ;; fic-face
   `(fic-face ((t (:foreground ,*bright-orange*))))

   ;; vim-empty-lines-mode
   `(vim-empty-lines-face ((t (:foreground ,*light-brown*))))

   ;; markdown-mode
   ;; `(markdown-header-delimiter-face ((t (:foreground ,*bluegreen*))))
   ;; `(markdown-header-face ((t (:foreground ,*yellow*))))
   ;; `(markdown-link-face ((t (:foreground ,*darkpurple*))))

   ;; flyspell-mode
   `(flyspell-incorrect
     ((t (:bold nil :underline t))))
   `(flyspell-duplicate
     ((t (:bold nil :underline t))))

   ;; avy
   `(avy-background-face
     ((t (:foreground ,*normal* :background ,*background*))))
   `(avy-lead-face-0
     ((t (:foreground ,*lightgreenblue* :background ,*greenblue* :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,*lightgreenblue* :background ,*greenblue* :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,*lightgreenblue* :background ,*greenblue* :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,*lightgreenblue* :background ,*greenblue* :weight bold))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'graham-crackers)

;; Local Variables:
;; no-byte-compile: t
;; End:
