;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; *****************************************************************************************
;;
;; jlucas: Dungeon adventuring theme
;;
;; *****************************************************************************************

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(defun myfunc ()
  (let ((x 0))
    (print "hi")))

(deftheme jlucas "Dungeon adventuring theme")

  (custom-theme-set-variables
    'jlucas
    '(linum-format " %7i "))

(let ((class '((class color) (min-colors 89)))
      (background "#1C1C1C")
      (selection  "#A8A8A8")
      (*paren-match* "#87ffaf")
      (*paren-mismatch* "#ff0000"))

  (custom-theme-set-faces
   'jlucas

   ;; Default colors
   ;; *****************************************************************************************

   `(default                          ((t (:foreground "#B2B2B2" :background ,background))))
   `(region                           ((t (:background ,selection                       ))))
   `(cursor                           ((t (:background "#ffffff"                        ))))
   `(fringe                           ((t (:background "#2f2f2f"   :foreground "#ffffff"))))
   `(linum                            ((t (:background ,background :foreground "#2f2f2f"))))
   `(minibuffer-prompt                ((t (:foreground "#9489C4"            :weight bold))))
   `(minibuffer-message               ((t (:foreground "#ffffff"                        ))))
   `(mode-line                        ((t (:foreground "#303030" :background "#d78700"  ))))
   `(mode-line-inactive               ((t (:foreground "#767676" :background "#303030"  ))))

   ;; Font lock faces
   ;; *****************************************************************************************
   
   `(font-lock-keyword-face           ((t (:foreground "#d78700" :weight bold))))
   `(font-lock-type-face              ((t (:foreground "#dadada"))))
   `(font-lock-function-name-face     ((t (:foreground "#d7af5f"))))
					;`(font-lock-constant-face          ((t (:foreground "#5f8787"))))
   `(font-lock-constant-face          ((t (:foreground "#8700d7"))))
   `(font-lock-variable-name-face     ((t (:foreground "#dadada"))))
   `(font-lock-builtin-face           ((t (:foreground "#87ffaf"))))
   `(font-lock-string-face            ((t (:foreground "#5f8787"))))
   `(font-lock-comment-face           ((t (:foreground "#767676"))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#767676"))))
   `(font-lock-doc-string-face        ((t (:foreground "#767676"))))

  ;; Rainbow delimiters
  ;; *****************************************************************************************

  `(rainbow-delimiters-depth-1-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-2-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-3-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-4-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-5-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-6-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-7-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-8-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-depth-9-face ((t (:foreground "#008787"))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground "#008787")))))

   ;; show-paren-mode
   `(show-paren-match-face ((t (:foreground ,*paren-match* :bold t))))
   `(show-paren-mismatch-face ((t (:foreground ,*paren-mismatch* :bold t))))
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************************

(provide-theme 'jlucas)

;; Local Variables:
;; no-byte-compile: t
;; End:
