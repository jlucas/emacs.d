;;;
;;; Theme
;;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(unless (boundp 'my/theme)
  (if (display-graphic-p)
      (setq my/theme 'ochreblue)
    (setq my/theme 'xterm16)))

(load-theme my/theme t)

;; From: http://emacs.stackexchange.com/questions/16313
(cond ((string= system-name "yew.106.net")
       (add-to-list 'default-frame-alist '(font . "Terminus-13")))
      (t
       (add-to-list 'default-frame-alist '(font . "Terminus-11"))))

;;; Theme packages

;; (use-package gotham-theme :ensure t)

;; (use-package minimal-theme :ensure t)

;; (use-package flatland-theme :ensure t)

;; (use-package arjen-grey-theme :ensure t)

;; (use-package gruvbox-theme :ensure t)

;; (use-package darktooth-theme :ensure t)

;; (use-package hemisu-theme :ensure t)

;; (use-package hydandata-light-theme :ensure t)

;; (use-package iodine-theme :ensure t)

;; (use-package jazz-theme :ensure t)

;; (use-package lavender-theme :ensure t)

;; (use-package mbo70s-theme :ensure t)

;; (use-package monochrome-theme :ensure t)

;; (use-package mustang-theme :ensure t)

;; (use-package mustard-theme :ensure t)

;; (use-package planet-theme :ensure t)

;; (use-package purple-haze-theme :ensure t)

;; (use-package slime-theme :ensure t)

;; https://github.com/owainlewis/emacs-color-themes

;; Set the font for the current frame
;; (set-frame-font "Terminus-12" nil t)

;; Set the font for future frames
;; (set-face-attribute 'default nil :font "Terminus-12")


