(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-auto-exclude-function nil)
 '(org-agenda-custom-commands
   (quote
    (("A" "Agenda & High Priority Tasks"
      ((agenda "" nil)
       (todo ""
             ((org-agenda-overriding-header "High Priority Tasks: ")
              (org-agenda-skip-function
               (quote
                (org-agenda-skip-entry-if
                 (quote regexp)
                 "\\* SOMEDAY"
                 (quote notregexp)
                 "\\=.*\\[#\\(A\\|B\\)\\]")))
              (org-agenda-sorting-strategy
               (quote
                (priority-down))))))
      nil)
     ("l" "All tasks" todo ""
      ((org-agenda-overriding-header "Unscheduled tasks: ")
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote regexp)
          "\\* SOMEDAY")))
       (org-agenda-sorting-strategy
        (quote
         (priority-down)))))
     ("w" "Waiting tasks" todo "WAITING"
      ((org-agenda-overriding-header "Waiting tasks:")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-up priority-down category-up)))))
     ("o" "Someday tasks" todo "SOMEDAY"
      ((org-agenda-overriding-header "Someday tasks:")))
     ("r" "All Review Entries" tags ":review:"
      ((org-agenda-skip-function
        (quote k/org-agenda-skip-expired-review-entry)))))))
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-files
   (quote
    ("~/Dropbox/PKG/Task/INBOX.org" "~/Dropbox/PKG/Task/Collector.org" "~/Dropbox/PKG/Task/PROJECTS.org")))
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c")
     (tags . "  %-11c"))))
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down user-defined-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode nil)
 '(org-agenda-tags-column -100)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-agenda-use-time-grid nil)
 '(org-archive-location "%s_archive::datetree")
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-attach-method (quote mv))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (sh . t)
     (ruby . t)
     (python . t)
     (C . t)
     (css . t)
     (java . t)
     (js . t))))
 '(org-beamer-frame-default-options "fragile")
 '(org-capture-templates
   (quote
    (("a" "Add Task" entry
      (file "~/Dropbox/PKG/Task/Collector.org")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("p" "Add Project" entry
      (file "~/Dropbox/PKG/Task/PROJECTS.org")
      "* %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:
** Notes
** Tasks" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/PKG/Task/Collector.org")
      "* %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:"))))
 '(org-clock-clocked-in-display nil)
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-mode-line-total (quote current))
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-persist t)
 '(org-clock-persist-file "~/.emacs.d/data/org-clock-save.el")
 '(org-clock-resolve-expert t)
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-crypt-disable-auto-save nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/PKG/Task/Collector.org")
 '(org-default-priority 67)
 '(org-directory "~/Dropbox/PKG/Task/")
 '(org-ditaa-jar-path "~/bin/DitaaEps.jar")
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "OUT")))
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-export-babel-evaluate nil)
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("linalg" "\\documentclass{article}
\\usepackage{linalgjh}
[DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-export-with-todo-keywords nil)
 '(org-extend-today-until 8)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line nil)
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-habit-today-glyph 45)
 '(org-hide-leading-stars t)
 '(org-id-locations-file "~/.emacs.d/data/org-id-locations")
 '(org-image-actual-width (quote (800)))
 '(org-indent-mode-turns-on-hiding-stars t)
 '(org-insert-heading-respect-content t)
 '(org-irc-link-to-logs t t)
 '(org-latex-default-packages-alist
(quote
 (("T1" "fontenc" t)
  ("" "fixltx2e" nil)
  ("" "graphicx" t)
  ("" "longtable" nil)
  ("" "float" nil)
  ("" "wrapfig" nil)
  ("" "rotating" nil)
  ("normalem" "ulem" t)
  ("" "amsmath" t)
  ("" "textcomp" t)
  ("" "marvosym" t)
  ("" "wasysym" t)
  ("" "amssymb" t)
  ("" "hyperref" nil)
  "\\tolerance=1000")))
 '(org-log-done (quote time))
 '(org-mobile-directory "~/Dropbox/PKG/MobileOrg")
 '(org-mobile-inbox-for-pull "~/Dropbox/PKG/Task/FROM-MOBILE.org")
 '(org-modules
(quote
 (org-gnus org-habit org-id org-info org-depend org-velocity)))
 '(org-publish-project-alist
(quote
 (("document" :base-directory "~/Dropbox/PKG/Document/" :base-extension "org" :publishing-directory "~/Dropbox/PKG/Publish/" :recursive t :publishing-function org-html-publish-to-html))))
 '(org-refile-targets (quote (("~/Dropbox/PKG/Task/PROJECTS.org" :level . 1))))
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-tab-acts-natively nil)
 '(org-startup-indented t)
 '(org-stuck-projects (quote ("STUCK" nil nil "")))
 '(org-tags-column -78)
 '(org-time-clocksum-use-fractional t)
 '(org-todo-keyword-faces
(quote
 (("TODO" :foreground "red" :weight bold)
  ("STARTED" :foreground "dark orange" :weight bold)
  ("WAITING" :foreground "medium blue" :weight bold)
  ("SOMEDAY" :foreground "dark blue" :weight bold)
  ("CANCELED" :foreground "gray" :weight bold)
  ("DONE" :foreground "ForestGreen" :weight bold))))
 '(org-todo-keywords
(quote
 ((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "SOMEDAY(o)" "|" "CANCELED(c)" "DONE(d)"))))
 '(org-todo-repeat-to-state "TODO")
 '(org-use-property-inheritance (quote ("AREA")))
 '(org-use-speed-commands t)
 '(org-use-tag-inheritance nil)
 '(org-velocity-always-use-bucket t)
 '(org-velocity-bucket "~/Dropbox/PKG/Task/PROJECTS.org")
 '(org-velocity-capture-templates
(quote
 (("v" "Velocity" entry
   (file "~/Dropbox/PKG/Task/INBOX.org")
   "* %:search
%i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \\\"uuidgen\\\"):CREATED:  %U
:END:" :prepend t))))
 '(org-velocity-exit-on-match t)
 '(org-velocity-force-new t)
 '(org-velocity-search-method (quote regexp))
 '(org-x-backends (quote (ox-org ox-redmine)))
 '(org-x-redmine-title-prefix-function (quote org-x-redmine-title-prefix))
 '(org-x-redmine-title-prefix-match-function (quote org-x-redmine-title-prefix-match)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-priority ((t (:inherit nil)))))
