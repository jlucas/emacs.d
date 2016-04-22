;;;
;;; Theme
;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(if (display-graphic-p)
    (load-theme 'ochreblue t)
  (load-theme 'xterm16 t))

(cond ((string= system-name "yew.106.net")
       (set-frame-font "Terminus-13" nil t))
      (t
       (set-frame-font "Terminus-11" nil t)))

;;(set-frame-font "Monospace-12" nil t)
;;(set-frame-font "snap-12" nil t)
;;(set-frame-font "Monospace-12" nil t)
;;(set-face-attribute 'default nil :height 100)

