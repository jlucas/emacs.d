
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load loosies from here
(add-to-list 'load-path "~/.emacs.d/vendor")

;; The rest of my user setup
(load "~/.emacs.d/user.el")

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a0f6c0774cb9ad3ac398f4ede0ffe8c0fb19887745783e9459d7c89327ee86a5" "0b2d512595d43d53dab6adf9fab9daa5e9d6d42c4c18c269354e0b8be96c50c1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
