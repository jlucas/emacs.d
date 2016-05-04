
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

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ba495a3cd1be02cfbb259f0b6e30c396a5511bf2acfdecf3541f4f2b91403a8c" "10b6f57c87df02b526df6fa5c1d92025148e98b3bd860f23de760c09685bdca6" "613f3b26da1310e615183d86a0568bc07b03e935b7210a09821181be9dc59d97" "a0f6c0774cb9ad3ac398f4ede0ffe8c0fb19887745783e9459d7c89327ee86a5" "0b2d512595d43d53dab6adf9fab9daa5e9d6d42c4c18c269354e0b8be96c50c1" default)))
 '(elscreen-tab-display-kill-screen nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
