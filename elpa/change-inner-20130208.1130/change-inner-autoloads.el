;;; change-inner-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "change-inner" "change-inner.el" (21899 45259
;;;;;;  141097 828000))
;;; Generated autoloads from change-inner.el

(autoload 'change-inner "change-inner" "\
Works like vim's ci command. Takes a char, like ( or \" and
kills the innards of the first ancestor semantic unit starting with that char.

\(fn ARG)" t nil)

(autoload 'copy-inner "change-inner" "\


\(fn)" t nil)

(autoload 'change-outer "change-inner" "\
Works like vim's ci command. Takes a char, like ( or \" and
kills the first ancestor semantic unit starting with that char.

\(fn ARG)" t nil)

(autoload 'copy-outer "change-inner" "\


\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; change-inner-autoloads.el ends here
