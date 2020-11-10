;; ------ org packages

;; Easy Template
;; http://emacs.stackexchange.com/questions/12841/quickly-insert-source-blocks-in-org-mode
;; http://orgmode.org/worg/org-contrib/babel/header-args.html
;; http://ehneilsen.net/notebook/orgExamples/org-examples.html#sec-2-1

;; export
;; http://rwx.io/blog/2016/03/11/Org-Export-Configurations/

(load (expand-file-name "org-settings" user-emacs-directory))

(general-define-key "C-c o" 'hydra-org/body)

(defhydra hydra-org (:color blue :hint nil :columns 4 :idle 0.3)
  "Org Helper"
  ;; ("a" org-agenda "Agenda")
  ("a" #'(lambda () (interactive) (org-agenda nil "z")) "My Agenda")
  ("z" #'(lambda () (interactive) (org-agenda nil "Z")) "Unscheduled Tasks")
  ;; ("c" org-capture "Capture")
  ;; ("p" my-org-agenda-current-project "Current Project")
  ("t" #'(lambda () (interactive) (org-capture nil "a")) "Capture Task")
  ("n" #'(lambda () (interactive) (org-capture nil "n")) "Capture Note")
  ("w" hydra-org-clock/body "Clock")
  ("l" org-pkb/show-all "PKB")
  ("d" org-pkb/show-daily "PKB Daily")
  ("r" #'(lambda () (interactive) (org-agenda nil "r")) "PKB Review")
  ("s" org-pkb/search "PKB Search")
  ;; ("t" org-pkb/collect "Collector")
  ("q" nil "Quit"))

(defhydra hydra-org-clock (:color blue :hint nil)
  "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    _i_n         _e_dit   _g_oto entry | (_z_)     _r_elative      ti_m_e
^ ^     _c_ontinue   _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
^ ^     _o_ut        ^ ^      _r_eport     |  ^ ^      _p_ause toggle
^ ^     ^ ^          ^ ^      ^ ^          |  ^ ^      _s_top
"
  ("i" org-clock-in)
  ("c" org-clock-in-last)
  ("o" org-clock-out)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands"))
  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ("s" org-timer-stop)
  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

;; remove buildin org-mode in load-path, to use newest org-mode downloaded from pacakge-install
;;(if (eq system-type 'darwin)
;;    (delete "/Applications/Emacs.app/Contents/Resources/lisp/org" load-path))
(use-package org
  :mode ("\\.org\\'"   . org-mode)
  :commands (org-mode
             org-capture)
  :config
  (progn
    ;; ------ basic settings
    (unbind-key "C-<tab>" org-mode-map)
    (unbind-key "C-y" org-mode-map)
    (unbind-key "C-j" org-mode-map)

    ;; presist org-clock
    (org-clock-persistence-insinuate)

    (defun my-org-mode-hook ()
      ;; (setq line-spacing 0.25)
      ;; (flyspell-mode 1)
      (buffer-face-mode 1)
      ;; (set (make-local-variable 'mantic-mode) nil)
      (turn-off-auto-fill)
      (company-mode -1)
      ;; (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)
      )
    (add-hook 'org-mode-hook 'my-org-mode-hook)

    (use-package org-bullets
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;; ------ org-babel
    ;; http://orgmode.org/guide/Working-With-Source-Code.html
    ;; (use-package ob-typescript)
    ;; (use-package ob-php)

    ;; ------ hydra
    (defhydra hydra-org-template (:color blue :hint nil)
      "
 _c_enter  _q_uote     _1_.emacs-lisp    _L_aTeX:
 _l_atex   _E_xample   _2_.shell         _i_ndex:
 _a_scii   _v_erse     _3_.python        _I_NCLUDE:
 _s_rc     _n_ote      _4_.scala         _H_TML:
 _h_tml    ^ ^         _5_.java          _A_SCII:
 ^ ^       ^ ^         _6_.javascript    ^ ^
 ^ ^       ^ ^         _7_.php           ^ ^
 ^ ^       ^ ^         _8_.sql           ^ ^
"
      ("s" (hot-expand "<s"))
      ("E" (hot-expand "<e"))
      ("q" (hot-expand "<q"))
      ("v" (hot-expand "<v"))
      ("n" (let (text) ; org-reveal speaker notes
             (when (region-active-p)
               (setq text (buffer-substring (region-beginning) (region-end)))
               (delete-region (region-beginning) (region-end)))
             (insert "#+BEGIN_NOTES\n\n#+END_NOTES")
             (forward-line -1)
             (when text (insert text))))
      ("c" (hot-expand "<c"))
      ("l" (hot-expand "<l"))
      ("h" (hot-expand "<h"))
      ("a" (hot-expand "<a"))
      ("L" (hot-expand "<L"))
      ("i" (hot-expand "<i"))
      ("1" (hot-expand "<s" "emacs-lisp"))
      ("2" (hot-expand "<s" "sh"))
      ("3" (hot-expand "<s" "python"))
      ("4" (hot-expand "<s" "scala"))
      ("5" (hot-expand "<s" "java"))
      ("6" (hot-expand "<s" "js"))
      ("7" (hot-expand "<s" "php"))
      ("8" (hot-expand "<s" "sql"))
      ("I" (hot-expand "<I"))
      ("H" (hot-expand "<H"))
      ("A" (hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("o" nil "quit"))

    (defun hot-expand (str &optional mod header)
      "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADERS: tag."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end))
          (deactivate-mark))
        (when header (insert "#+HEADERS: " header))
        (insert str)
        (org-try-structure-completion)
        (when mod (insert mod) (forward-line))
        (when text (insert text))))

    (define-key org-mode-map "<"
      (lambda () (interactive)
        (if (or (region-active-p) (looking-back "^"))
            (hydra-org-template/body)
          (self-insert-command 1))))

    ;; ------ hack
    (defun my-auto-save-org-files ()
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'org-mode)
            (if (and (buffer-modified-p) (buffer-file-name))
                (progn 
                  (message (concat "Saving changes of org file: " (buffer-file-name)))
                  (call-interactively 'save-buffer)))))))
    (run-with-idle-timer 10 t 'my-auto-save-org-files)

    ;; (defadvice org-refile-get-location (around my-org-refile-target activate)
    ;;   "Refile entry to specific category such as Notes or Tasks"
    ;;   (let* ((it ad-do-it)
    ;;          (file (nth 1 it))
    ;;          (re (nth 2 it))
    ;;          (pos (nth 3 it))
    ;;          (target-headline (if (member (if (equal major-mode 'org-agenda-mode)
    ;;                                           "TODO"
    ;;                                         (org-get-todo-state))
    ;;                                       '("NOTE" "" nil))
    ;;                               "Notes"
    ;;                             "Tasks")))
    ;;     (message target-headline)
    ;;     (when (string-match-p "Projects.org$" file)
    ;;       (save-excursion
    ;;         (with-current-buffer (find-file-noselect file)
    ;;           (goto-char pos)
    ;;           (re-search-forward (concat "^\\*\\* " target-headline))
    ;;           (setcar (nthcdr 3 it) (point)))))
    ;;     it))

    ;; (defcustom org-my-export-output-directory-prefix "~/Dropbox/PKB/Publish/export_"
    ;;   "prefix of directory used for org-mode export"
    ;;   :type 'string
    ;;   :group 'org-mine)

    ;; (defadvice org-export-output-file-name (before org-add-export-dir activate)
    ;;   "Modifies org-export to place exported files in a different directory"
    ;;   (when (not pub-dir)
    ;;     (setq pub-dir (concat org-my-export-output-directory-prefix (substring extension 1)))
    ;;     (when (not (file-directory-p pub-dir))
    ;;       (make-directory pub-dir))))

    (use-package toc-org
      :config
      (add-hook 'org-mode-hook 'toc-org-enable))

    ))

;; === Start of org-agenda ===

(use-package org-agenda :ensure nil
  :commands (my-org-agenda-current-project
             org-agenda)
  :demand t
  :config
  (progn
    (setq org-agenda-files (append
                            '("~/Dropbox/PKB/Task/QuickTasks.org" "~/Dropbox/PKB/Task/QuickCaptures.org")
                            '("~/Dropbox/PKB/Task/Projects.org")
                            ;; (directory-files-recursively "~/Dropbox/PKB/Document" t org-agenda-file-regexp)
                            ))

    ;; Org-Agenda shortcut https://orgmode.org/manual/Agenda-Commands.html
    (setq org-agenda-custom-commands
          '(
            ("z" "My Agenda"
             ((agenda ""
                      ((org-super-agenda-groups
                        '((:log t)  ; Automatically named "Log"
                          (:habit t)
                          (:name "Time Grid"
                                 :time-grid t)
                          (:name "Scheduled Today"
                                 :scheduled today)
                          (:name "Due today"
                                 :deadline today)
                          (:name "Overdue"
                                 :deadline past)
                          (:name "Due soon"
                                 :deadline future)
                          (:name "Waiting..."
                                 :todo "WAITING"
                                 :order 98)
                          (:name "Scheduled earlier"
                                 :scheduled past)))))
              ;; (org-agenda-mode-hook
              ;;  (lambda ()
              ;;    (org-mac-iCal)))
              ))
            ("Z" "Unshceduled Tasks (Group)"
             ((alltodo ""
                       ((org-agenda-overriding-header "Unshceduled Tasks: ")
                        (org-super-agenda-groups
                         '((:name "High Priority Tasks" :priority>= "B")
                           (:name "Someday Tasks" :todo "SOMEDAY" :order 99)
                           (:auto-group t)))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                        ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                        (org-agenda-sorting-strategy '(todo-state-up priority-down))))))
            ))

            ;; ("z" "My Agenda"
            ;;  ((agenda ""
            ;;           ((org-agenda-start-on-weekday nil)
            ;;            (org-agenda-span 3)
            ;;            (org-agenda-start-day "-1d")))
            ;;   (todo "TODO"
            ;;            ((org-agenda-overriding-header "Group Of Tasks [sort by priority]: ")
            ;;             (org-super-agenda-groups
            ;;              '((:auto-group t)))
            ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
            ;;             (org-agenda-skip-function
            ;;              '(org-agenda-skip-entry-if 'notregexp " \\[#\\(A\\|B\\)\\] "))
            ;;             (org-agenda-sorting-strategy
            ;;              '(todo-state-up priority-down))))))
            ;; ("A" "Agenda & Tasks [sort by priority]"
            ;;  ((agenda "" ((org-agenda-span 1)
            ;;               (org-deadline-warning-days 7)
            ;;               ;; (org-agenda-todo-keyword-format "[ ]")
            ;;               (org-agenda-scheduled-leaders '("" ""))
            ;;               (org-agenda-prefix-format "%t%s")))
            ;;   (todo "TODO"
            ;;         ((org-agenda-overriding-header "Tasks [sort by priority]: ")
            ;;          ;; (org-agenda-prefix-format "[ ] %T: ")
            ;;          ;; (org-agenda-todo-keyword-format "")
            ;;          ;; (org-agenda-skip-function
            ;;          ;;  ;; '(org-agenda-skip-entry-if 'regexp "\\* SOMEDAY" 'notregexp "\\=.*\\[#\\(A\\|B\\)\\]")
            ;;          ;;  '(org-agenda-skip-entry-if 'regexp "\\* SOMEDAY" 'scheduled)
            ;;          ;;  )
            ;;          (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
            ;;          (org-agenda-sorting-strategy
            ;;           '(todo-state-up priority-down)))))
            ;;  (;; (org-agenda-with-colors nil)
            ;;   ;; (org-agenda-compact-blocks t)
            ;;   ;; (org-agenda-remove-tags t)
            ;;   (ps-number-of-columns 2)
            ;;   (ps-landscape-mode t)))
            ;; ("l" "All tasks" todo ""
            ;;  ((org-agenda-overriding-header "Unscheduled tasks: ")
            ;;   (org-agenda-skip-function
            ;;    '(org-agenda-skip-entry-if 'regexp "\\* SOMEDAY"))
            ;;   (org-agenda-sorting-strategy
            ;;    '(priority-down))))
            ;; ("w" "Waiting tasks" todo "WAITING"
            ;;  ((org-agenda-overriding-header "Waiting tasks:")
            ;;   (org-agenda-sorting-strategy
            ;;    '(todo-state-up priority-down category-up))))
            ;; ("o" "Someday tasks" todo "SOMEDAY"
            ;;  ((org-agenda-overriding-header "Someday tasks:")))

    (use-package org-super-agenda
      :hook ((org-agenda-mode . org-super-agenda-mode)))

    (defun my-org-agenda-mode-hook ()
      ;; (setq line-spacing 0.25)
      (hl-line-mode 1)
      (origami-mode 1))
    (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

    ;; (let ((map org-agenda-mode-map))
    ;;   (bind-key "\C-n" 'next-line map)
    ;;   (bind-key "\C-p" 'previous-line map)
    ;;   (bind-key "g" 'org-agenda-redo map)
    ;;   (bind-key "f" 'org-agenda-date-later map)
    ;;   (bind-key "b" 'org-agenda-date-earlier map)
    ;;   (bind-key "r" 'org-agenda-refile map)
    ;;   (bind-key "SPC" 'org-agenda-tree-to-indirect-buffer  map)
    ;;   (bind-key "F" 'org-agenda-follow-mode map)
    ;;   (bind-key "q" 'delete-window map)
    ;;   (bind-key "M-p" 'org-agenda-earlier map)
    ;;   (bind-key "M-n" 'org-agenda-later map)
    ;;   (bind-key ">" 'org-agenda-filter-by-top-headline map)

    ;;   (unbind-key "M-m" map))

    ;; https://github.com/IvanMalison/org-projectile
    (use-package org-projectile
      :after projectile
      ;; :commands (org-projectile:project-todo-completing-read)
      :disabled t
      :config
      (progn
        ;; (setq org-projectile:projects-file "~/Dropbox/PKB/Task/PROJECTS_TODO.org")
        (org-projectile-per-project)
        (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
        (add-to-list 'org-capture-templates
                     (org-projectile-project-todo-entry
                      "l" "* TODO %? %a\n" "Linked Project TODO"))
        (add-to-list 'org-capture-templates (org-projectile-project-todo-entry "p"))

        (add-to-list 'org-agenda-custom-commands
                     '("p" "Current Project tasks" todo ""
                       ((org-agenda-overriding-header "Current Project Tasks:")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'regexp "\\* SOMEDAY"))
                        (org-agenda-sorting-strategy
                         '(priority-down))))
                     t)

        (defun my-org-agenda-current-project (arg)
          "Show agenda view for current project"
          (interactive "P")
          (let ((org-agenda-files
                 (list (expand-file-name
                        (funcall org-projectile-project-name-to-org-file
                                 (projectile-project-name))))))
            (org-agenda arg "p" nil)))
        ))

    (use-package org-pkb :ensure nil
      :demand
      :commands (org-pkb/show-all
                 org-pkb/show-daily
                 org-pkb/search
                 org-pkb/org-agenda-skip-expired-review-entry)

      :config
      (progn
        ;; (push '("Collector.org" :position bottom :height 15 :stick t)
        ;;       popwin:special-display-config)

        ;; (add-hook 'org-after-todo-state-change-hook 'org-pkb/archive-my-daily-jobs)
        ;; (add-hook 'org-clock-out-hook 'org-pkb/archive-my-daily-jobs)

        (org-defkey org-agenda-mode-map "D" 'org-pkb/org-agenda-magic-done)
        (add-to-list 'org-agenda-custom-commands
                     '("r" "All Review Entries" tags ":review:"
                       ((org-agenda-files (directory-files-recursively org-pkb-doc-dir t org-agenda-file-regexp))
                        (org-agenda-overriding-header "Review Entries: ")
                        (org-agenda-prefix-format "  %-12:c")
                        (org-agenda-remove-tags t)
                        (org-agenda-skip-function 'org-pkb/org-agenda-skip-expired-review-entry)))
                     t)
        ))
    ))

;; === End of org-agenda ===

(use-package org-mobile :ensure nil
  :defer t)

(use-package htmlize)

;;; org.el ends here
