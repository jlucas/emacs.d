;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'package)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;(load (concat (file-name-as-directory (getenv "HOME")) ".emacs.d/user.el"))
(load "~/.emacs.d/user.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "e4a2059d3fe38c44a373f1f7e0c3d9f282892801e19bb4e6ad73ba0c7a5eaf73" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "7c2fdd4d512b1fe016292708c454031979e5c38a11a7365efdd12aa4c6ad000e" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "34378a4f429d8413b32f7c6d7ee2844dc5395f99881ea540dde6168383fba3fe" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "fab480f4480fea6315373b7de4b97848b796efa75fd1a52039c5ec44dff12940" "2258c76e10cd14e83f842cd83735b2ad4cbadd7a62ac4b4792fa61b104517ea0" "7bf64a1839bf4dbc61395bd034c21204f652185d17084761a648251041b70233" "d996dc586acf21a851398ab85b793cf9c70f4e8ae9c5aa42e421b9208b2156cf" "18d2e7f7cf98f7d6166e9103f352205eaf159a76e046ad4b6b742757fa6b945d" "5d7e424c2145e29aaefdf116d91ae9eb3c6f71c87a7b0119f7efefe8499a2901" "262de902a25d4741a23589874e568c894688e3ac4d3478d92e6e4236fee0c158" "c33fcc4ed69dff130374106157bd621fadfcce877347364b52defe6cd6b49349" "b707b814bd29743db726f5dff1f753ac36fef0a760202813b07a107d074404e4" "7616b63997f63234cd2b996cfd3c27d795725e8479cd2d013ade419829a2b14e" "32f3976eb4888314addd119011fec64257da49bc1a5ae6a12aee0dc44904acc3" "e74fb38aa33aa157814953ffb1dd4616de3f72237414b752e92349acc8a434e3" "e51d81159ef8f934072a8050aecf68bff0d6f54712418685dd6b97743ad289ba" "339dff1ea2739650fd824c1cdb7e01421b7a0384e08d6e3595ca069e995b46f3" "7cb2a7f19d61f8bae3619484e867fd54854d91f9f9e2d17aed6670586b13bad7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
