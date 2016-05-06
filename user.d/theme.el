;;;
;;; Theme
;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(if (display-graphic-p)
    (load-theme 'ochreblue t)
  (load-theme 'xterm16 t))

;; From: http://emacs.stackexchange.com/questions/16313
(cond ((string= system-name "yew.106.net")
       (add-to-list 'default-frame-alist '(font . "Terminus-13")))
      (t
       (add-to-list 'default-frame-alist '(font . "Terminus-11"))))

;; Set the font for the current frame
;; (set-frame-font "Terminus-12" nil t)

;; Set the font for future frames
;; (set-face-attribute 'default nil :font "Terminus-12")


