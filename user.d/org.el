;;;
;;; org-mode setup
;;;

;; Support embedding the following languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . t)
   (dot . t)
   (sqlite . t)
   (lisp . t)))
;; Prompt for a comment and add a datestamp to DONE items.
(setq org-log-done 'note)
;; http://orgmode.org/manual/Clean-view.html
;; If you decide you don't like this, you can enable it on specific
;; org files by adding the text "#+STARTUP: indent" somewhere in the
;; file
(setq org-startup-indented t)
;; Agenda files
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
;; From: http://orgmode.org/worg/org-faq.html#orgheadline41
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)
;; From: http://orgmode.org/manual/Workflow-states.html
;(setq org-todo-keywords
;      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))
                           ("~/src/lispmud/lispmud.org" . (:maxlevel . 6))))
;; From: http://orgmode.org/manual/Internal-archiving.html#Internal-archiving
;; Show archived items when cycling with S-TAB
;; FIXME: Doesn't work in console emacs
;(setq org-columns-skip-archived-trees nil)
(setq org-todo-keywords
      '((sequence "TASK(t)"
                  "INPROGRESS(i)"
                  "SCHEDULED(s)"
                  "FEEDBACK(f)"
                  "ONHOLD(h)"
                  "BLOCKED(b)"
                  "DELEGATED(g)"
                  "|"
                  "DONE(d)"
                  "CLOSED(c)"
                  "CANCELLED(a)")))
(setq org-tags-exclude-from-inheritance '("prj")
      org-stuck-projects '("+prj/-MAYBE-DONE"
                           ("TODO" "TASK") ()))
(setq org-agenda-custom-commands
      '(("h" "Work todos" tags-todo
         "-personal-doat={.+}-dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled t)))
        ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("A" "Work todos with doat or dowith" tags-todo
         "-personal+doat={.+}|dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("j" "TODO dowith and TASK with"
         ((org-sec-with-view "TODO dowith")
          (org-sec-where-view "TODO doat")
          (org-sec-assigned-with-view "TASK with")
          (org-sec-stuck-with-view "STUCK with")))
        ("J" "Interactive TODO dowith and TASK with"
         ((org-sec-who-view "TODO dowith")))))
(eval-after-load 'org-secretary
  '(define-key org-mode-map (kbd "C-c w") 'org-sec-set-with))
(eval-after-load 'org-secretary
  '(define-key org-mode-map (kbd "C-c W") 'org-sec-set-where))
;(load "~/.emacs.d/vendor/org-secretary.el")
(setq org-sec-me "jlucas")
