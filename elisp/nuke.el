;; Integrate with server in nuke.vim
;; https://github.com/heavyimage/nuke.vim

(defun write-nuke ()
  (interactive)
  (save-excursion
    (if (not (region-active-p))
        (progn
          (backward-paragraph)
          (set-mark-command nil)
          (forward-paragraph)))
    (write-region (region-beginning) (region-end) "/tmp/file")
    (call-process "/bin/sh" nil nil nil "-c" "echo -n \"/tmp/file\" | nc localhost 10191")
    (deactivate-mark
     t)))

(define-key python-mode-map (kbd "C-c C-c") 'write-nuke)
