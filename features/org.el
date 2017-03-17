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
  ("o" my-org-agenda-startup "StartUp")
  ("a" org-agenda "Agenda")
  ("c" org-capture "Capture")
  ("w" hydra-org-clock/body "Clock")
  ("l" org-kb/show-all "KB")
  ("d" org-kb/show-daily "Daily")
  ("s" org-kb/search "Search Documents")
  ("t" org-kb/collect "Collector")
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

(use-package org
  :mode ("\\.org\\'"   . org-mode)
  :commands (org-mode
             org-capture)
  :config
  (progn
    ;; ------ basic settings
    (unbind-key "C-<tab>" org-mode-map)
    (unbind-key "C-y" org-mode-map)

    (defun my-org-mode-hook ()
      ;; (setq line-spacing 0.25)
      ;; (flyspell-mode 1)
      (buffer-face-mode 1)
      ;; (set (make-local-variable 'mantic-mode) nil)
      (turn-off-auto-fill)
      (company-mode -1))
    (add-hook 'org-mode-hook 'my-org-mode-hook)

    (use-package org-bullets
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;; ------ org-babel
    ;; http://orgmode.org/guide/Working-With-Source-Code.html
    (use-package ob-typescript)
    (use-package ob-php)

    ;; ------ hydra
    (defhydra hydra-org-template (:color blue :hint nil)
      "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_hp           _i_ndex:
 _a_scii   _v_erse     _S_cala         _I_NCLUDE:
 _s_rc     _n_ote      _P_ython        _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
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
      ("e" (hot-expand "<s" "emacs-lisp"))
      ("p" (hot-expand "<s" "php"))
      ("S" (hot-expand "<s" "scala"))
      ("P" (hot-expand "<s" "python"))
      ("j" (hot-expand "<s" "js"))
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
                (save-buffer))))))
    (run-with-idle-timer 25 t 'my-auto-save-org-files)

    (defadvice org-refile-get-location (around my-org-refile-target activate)
      "Refile entry to specific category such as Notes or Tasks"
      (let* ((it ad-do-it)
             (file (nth 1 it))
             (re (nth 2 it))
             (pos (nth 3 it))
             (target-headline (if (member (if (equal major-mode 'org-agenda-mode)
                                              "TODO"
                                            (org-get-todo-state))
                                          '("NOTE" "" nil))
                                  "Notes"
                                "Tasks")))
        (message target-headline)
        (when (string-match-p "PROJECTS.org$" file)
          (save-excursion
            (with-current-buffer (find-file-noselect file)
              (goto-char pos)
              (re-search-forward (concat "^\\*\\* " target-headline))
              (setcar (nthcdr 3 it) (point)))))
        it))

    ;; (defcustom org-my-export-output-directory-prefix "~/Dropbox/PKG/Publish/export_"
    ;;   "prefix of directory used for org-mode export"
    ;;   :type 'string
    ;;   :group 'org-mine)

    ;; (defadvice org-export-output-file-name (before org-add-export-dir activate)
    ;;   "Modifies org-export to place exported files in a different directory"
    ;;   (when (not pub-dir)
    ;;     (setq pub-dir (concat org-my-export-output-directory-prefix (substring extension 1)))
    ;;     (when (not (file-directory-p pub-dir))
    ;;       (make-directory pub-dir))))
    ))

(use-package org-agenda :ensure nil
  :commands (my-org-agenda-startup org-agenda)
  :config
  (progn
    (defun my-org-agenda-mode-hook ()
      ;; (setq line-spacing 0.25)
      (hl-line-mode 1))
    (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

    (defun my-org-agenda-startup ()
      (interactive)
      (let ((buf (get-buffer "*Org Agenda*")))
        (if buf
            (let ((wind (get-buffer-window buf)))
              (if wind
                  (when (called-interactively-p 'any)
                    (select-window wind))
                (if (called-interactively-p 'any)
                    (select-window (display-buffer buf t t))
                  (display-buffer buf))))
          ;;(org-fit-window-to-buffer))
          (progn
            (call-interactively '(lambda (&optional arg)
                                   (interactive "P")
                                   (org-agenda arg "A")))))))

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
      :config
      (progn
        ;; (setq org-projectile:projects-file "~/Dropbox/PKG/Task/PROJECTS_TODO.org")
        (org-projectile:per-repo)
        (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
        (add-to-list 'org-capture-templates
                     (org-projectile:project-todo-entry
                      "l" "* TODO %? %a\n" "Linked Project TODO"))
        (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
        ))
    ))

(use-package org-mobile :ensure nil
  :defer t)

(use-package org-knowledgebase :ensure nil
  :commands (org-kb/show-all
             org-kb/show-daily
             org-kb/search
             org-kb/collect)
  :config
  (progn
    ;; (push '("Collector.org" :position bottom :height 15 :stick t)
    ;;       popwin:special-display-config)

    (setq org-agenda-files
          (append org-agenda-files
                  (directory-files-recursively org-kb-doc-dir t org-agenda-file-regexp)))

    (add-hook 'org-after-todo-state-change-hook 'org-kb/archive-my-daily-jobs)
    (add-hook 'org-clock-out-hook 'org-kb/archive-my-daily-jobs)

    (org-defkey org-agenda-mode-map "D" 'org-kb/org-agenda-magic-done)
    (add-to-list 'org-agenda-custom-commands
                 '("q" "All Review Entries" tags ":review:"
                   ((org-agenda-skip-function ;; (org-agenda-files (list org-my-knowledgebase-dir))
                     'org-kb/org-agenda-skip-expired-review-entry))) t)
    ))

;;; org.el ends here
