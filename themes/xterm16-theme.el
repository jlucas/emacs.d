
(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

;; this is great
(deftheme xterm16 "A dark color theme for Emacs")

(let ((*background*         "#1c1c1c")
      (*comments*           "#8787af")
      (*constant*           "#8700d7")
      (*current-line*       "#151515")
      (*cursor-underscore*  "#FFFAAA")
      (*keywords*           "#0087af")
      (*line-number*        "#afaf87")
      (*method-declaration* "#a8a8a8")
      (*mode-line-bg*       "#777")
      (*mode-inactive-bg*   "#555")
      (*mode-line-fg*       "#333333")
      (*mode-inactive-fg*   "#222")
      (*normal*             "#a8a8a8")
      (*number*             "#87afaf")
      (*operators*          "#00af87")
      (*warning*            "#9d7a04")
      (*regexp*             "#9d7")
      (*string*             "#87afaf")
      (*variable*           "#00afaf")
      (*visual-selection*   "#444")
      (*paren*              "#00af87")
      (*paren-match*        "#87ffaf")
      (*paren-mismatch*     "#ff0000"))

  ;(defgroup paren-face nil
  ;  "Face for parentheses in Lisp modes"
  ;  :group 'font-lock-extra-types
  ;  :group 'faces)
  ;
  ;(defface parenthesis
  ;  '((t (:foreground "green")))
  ;  :group 'basic-faces)
  
  (custom-theme-set-faces
   'xterm16

   ;`(default                          ((t (:foreground "#B2B2B2" :background ,background))))
   ;`(region                           ((t (:background ,selection                       ))))
   ;`(cursor                           ((t (:background "#ffffff"                        ))))
   ;`(fringe                           ((t (:background "#2f2f2f"   :foreground "#ffffff"))))
   ;`(linum                            ((t (:background ,background :foreground "#2f2f2f"))))
   ;`(minibuffer-prompt                ((t (:foreground "#9489C4"            :weight bold))))
   ;`(minibuffer-message               ((t (:foreground "#ffffff"                        ))))
   ;`(mode-line                        ((t (:foreground "#303030" :background "#d78700"  ))))
   ;`(mode-line-inactive               ((t (:foreground "#767676" :background "#303030"  ))))

   `(default ((t (:foreground ,*normal* :background ,*background*))))
   `(bold ((t (:bold t))))
   ;`(fringe                           ((t (:background "#2f2f2f"   :foreground "#ffffff"))))
   `(linum ((t (:background ,*background* :foreground ,*line-number* :bold nil :underline nil))))
   `(button ((t (:foreground ,*keywords* :underline t))))
   `(default ((t (:background ,*background* :foreground ,*normal*))))
   `(header-line ((t (:background ,*mode-line-bg* :foreground ,*normal*)))) ;; info header
   `(highlight ((t (:background ,*current-line*))))
   `(highlight-face ((t (:background ,*current-line*))))
   `(hl-line ((t (:background ,*current-line* :underline t))))
   `(info-xref ((t (:foreground ,*keywords* :underline t))))
   `(region ((t (:background ,*visual-selection*))))
   `(underline ((nil (:underline t))))
   `(paren ((t (:foreground ,*paren*))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,*operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,*comments*))))
   `(font-lock-comment-face ((t (:foreground ,*comments*))))
   `(font-lock-constant-face ((t (:foreground ,*constant*))))
   `(font-lock-doc-face ((t (:foreground ,*string*))))
   `(font-lock-doc-string-face ((t (:foreground ,*string*))))
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
   `(cursor ((t (:background, *cursor-underscore*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *mode-inactive-bg*)))) ;; between splits

   ;; search
   `(isearch ((t (:background, *regexp* :foreground, *visual-selection*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))

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

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'xterm16)

;; Local Variables:
;; no-byte-compile: t
;; End:
