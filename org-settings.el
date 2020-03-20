(custom-set-variables
 '(org-x-redmine-title-prefix-match-function (quote org-x-redmine-title-prefix-match))
 '(org-x-redmine-title-prefix-function (quote org-x-redmine-title-prefix))
 '(org-x-backends (quote (ox-org ox-redmine)))
 '(org-velocity-search-method (quote regexp))
 '(org-velocity-force-new t)
 '(org-velocity-exit-on-match t)
 '(org-velocity-capture-templates
   (quote
    (("v" "Velocity" entry
      (file "~/Dropbox/PKG/Task/QuickTasks.org")
      "* %:search
%i%?
:PROPERTIES:
:ID:       %(shell-command-to-string \\\"uuidgen\\\"):CREATED:  %U
:END:" :prepend t))))
 '(org-velocity-bucket "~/Dropbox/PKG/Task/QuickTasks.org")
 '(org-velocity-always-use-bucket t)
 '(org-use-tag-inheritance nil)
 '(org-use-speed-commands t)
 '(org-use-property-inheritance (quote ("AREA")))
 '(org-todo-repeat-to-state "TODO")
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "SOMEDAY(s)" "|" "CANCELED(c)" "DONE(d)"))))
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "red" :weight bold)
     ("SOMEDAY" :foreground "dark blue" :weight bold)
     ("CANCELED" :foreground "gray" :weight bold)
     ("DONE" :foreground "ForestGreen" :weight bold))))
 '(org-time-clocksum-use-fractional t)
 '(org-tags-column -78)
 '(org-stuck-projects (quote ("STUCK" nil nil "")))
 '(org-startup-indented t)
 '(org-src-window-setup (quote current-window))
 '(org-src-tab-acts-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-lang-modes
   (quote
    (("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . fundamental)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("js" . js2))))
 '(org-src-fontify-natively t)
 '(org-reverse-note-order t)
 '(org-return-follows-link t)
 '(org-refile-targets
   (quote
    (("~/Dropbox/PKG/Task/QuickTasks.org" :tag . "PROJECT"))))
 '(org-publish-project-alist
   (quote
    (("document" :base-directory "~/Dropbox/PKG/Document/" :base-extension "org" :publishing-directory "~/Dropbox/PKG/Publish/" :recursive t :publishing-function org-html-publish-to-html)
     ("org-blog" :base-directory "~/Dropbox/PKG/Blog/org/" :base-extension "org" :publishing-directory "~/Dropbox/PKG/Blog/jekyll/" :recursive t :publishing-function org-html-publish-to-html :headline-levels 4 :html-extension "html" :body-only t)
     ("org-blog-static" :base-directory "~/Dropbox/PKG/Blog/org/" :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php" :publishing-directory "~/Dropbox/PKG/Blog/" :recursive t :publishing-function org-publish-attachment)
     ("blog" :components
      ("org-blog" "org-blog-static")))))
 '(org-projectile:projects-file "~/Dropbox/PKG/Task/org-projectile-tasks.org")
 '(org-modules
   (quote
    (org-gnus org-habit org-id org-info org-depend org-velocity)))
 '(org-mobile-inbox-for-pull "~/Dropbox/PKG/Task/FROM-MOBILE.org")
 '(org-mobile-directory "~/Dropbox/PKG/MobileOrg")
 '(org-log-done (quote time))
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
 '(org-irc-link-to-logs t t)
 '(org-insert-heading-respect-content t)
 '(org-indent-mode-turns-on-hiding-stars t)
 '(org-image-actual-width (quote (800)))
 '(org-id-locations-file "~/.emacs.d/.data/org-id-locations")
 '(org-html-validation-link nil)
 '(org-hide-leading-stars t)
 '(org-habit-today-glyph 45)
 '(org-habit-preceding-days 42)
 '(org-footnote-section nil)
 '(org-fontify-whole-heading-line nil)
 '(org-fontify-done-headline t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-extend-today-until 8)
 '(org-export-with-todo-keywords nil)
 '(org-export-use-babel nil)
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
 '(org-enforce-todo-dependencies t)
 '(org-edit-src-content-indentation 0)
 '(org-edit-src-auto-save-idle-delay 0)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "OUT")))
 '(org-ditaa-jar-path "~/bin/DitaaEps.jar")
 '(org-directory "~/Dropbox/PKG/")
 '(org-default-priority 67)
 '(org-default-notes-file "~/Dropbox/PKG/Task/QuickNotes.org")
 '(org-deadline-warning-days 14)
 '(org-cycle-global-at-bob t)
 '(org-crypt-disable-auto-save nil)
 '(org-confirm-shell-link-function nil)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-babel-evaluate nil)
 '(org-completion-use-ido t)
 '(org-clock-resolve-expert t)
 '(org-clock-persist-file "~/.emacs.d/.data/org-clock-save.el")
 '(org-clock-persist t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-modeline-total (quote current))
 '(org-clock-mode-line-total (quote current))
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-in-switch-to-state "TODO")
 '(org-clock-in-resume t)
 '(org-clock-idle-time 10)
 '(org-clock-clocked-in-display nil)
 '(org-capture-templates
   (quote
    (("a" "Add Task" entry
      (file "~/Dropbox/PKG/Task/QuickTasks.org")
      "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("p" "Add Project" entry
      (file "~/Dropbox/PKG/Task/QuickTasks.org")
      "* %? :PROJECT:
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:" :prepend t)
     ("n" "Note" entry
      (file "~/Dropbox/PKG/Task/QuickNotes.org")
      "* %?"))))
 '(org-beamer-frame-default-options "fragile")
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (ruby . t)
     (python . t)
     (C . t)
     (css . t)
     (java . t)
     (js . t))))
 '(org-attach-method (quote mv))
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-archive-location "%s_archive::datetree")
 '(org-agenda-use-time-grid nil)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-agenda-tags-column -100)
 '(org-agenda-start-with-log-mode nil)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-span (quote day))
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down user-defined-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-scheduled-text "")
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-persistent-filter t)
 '(org-agenda-ndays 1)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-inhibit-startup t)
 '(org-agenda-include-diary t)
 '(org-agenda-fontify-priorities t)
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-custom-commands-contexts (quote (("p" (projectile-project-p)))))
 '(org-agenda-auto-exclude-function nil)
 '(org-adapt-indentation nil)
 '(org-M-RET-may-split-line (quote ((headline) (default . t)))))

(custom-set-faces)
