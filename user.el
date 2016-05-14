
(defun jl/load-if-readable (filepath)
  (if (file-readable-p filepath)
      (load filepath)))

;; Full paths so I can use ffap to jump to them
(let ((config-files '("~/.emacs.d/user.d/packages.el"
					  "~/.emacs.d/user.d/core.el"
					  "~/.emacs.d/user.d/theme.el"
					  "~/.emacs.d/user.d/file-format.el"
					  "~/.emacs.d/user.d/bind-user.el"
					  "~/.emacs.d/user.d/bind-global-override.el"
					  "~/.emacs.d/user.d/local.el")))
  (dolist (filepath config-files)
    (jl/load-if-readable filepath)))

(defun vim ()
  (interactive)
  (load "~/.emacs.d/user.d/evil.el"))

;; Always load this file as an entry point into my config
(find-file load-file-name)

;; Use TCP for emacs daemon
(setq server-use-tcp t)
;; The socket used for TCP is subject to tmpwatch cleanup as its mtime
;; is never updated after the initial creation.  Redirecting elsewhere
;; seems to involve setting TMPDIR at emacs invocation time.
;; See: http://stackoverflow.com/questions/22071093
