;; Set up LOAD-PATH
(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(defun jl/load-if-readable (filepath)
  (if (file-readable-p filepath)
      (load filepath)))

(defun jl/load-maybe (file)
  (condition-case nil
      (load file)
    (error (warn (format "jl/load-maybe: File not found: %s" file)))))

;; Full paths so I can use ffap to jump to them
(let ((config-files '("~/.emacs.d/user.d/packages.el"
                      "~/.emacs.d/user.d/core.el"
                      "~/.emacs.d/user.d/theme.el"
                      "~/.emacs.d/user.d/file-format.el"
                      "~/.emacs.d/user.d/bind-user.el"
                      "~/.emacs.d/user.d/bind-global-override.el"
                      "~/.emacs.d/user.d/gnus.el"
                      "~/.emacs.d/user.d/local.el")))
  (dolist (filepath config-files)
    (jl/load-if-readable filepath)))

;; Always load this file as an entry point into my config
(find-file load-file-name)

;; Use TCP for emacs daemon on Windows
(if (string-equal system-type "windows-nt")
    (setq server-use-tcp t))

;; (setq server-socket-dir (format "~/.emacs.d/server/%s" (system-name)))

;; The socket used for TCP is subject to tmpwatch cleanup as its mtime
;; is never updated after the initial creation.  Redirecting elsewhere
;; seems to involve setting TMPDIR at emacs invocation time.
;; See: http://stackoverflow.com/questions/22071093
