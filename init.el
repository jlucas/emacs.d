
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq custom-file "custom.el")
(load custom-file 'noerror)

;; Load loosies from here
(add-to-list 'load-path "~/.emacs.d/vendor")

;; The rest of my user setup
(load "~/.emacs.d/user.el")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
