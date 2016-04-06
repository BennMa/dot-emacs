(require 'cl)
(require 'use-package)
(load "org-settings")

(use-package org
  :bind (("M-A"   . jump-to-org-agenda)
         ("C-c a" . org-agenda)
         ("M-m"   . org-smart-capture)
         ("C-. n" . org-velocity-read)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link))
  :bind* ("C-t"   . org-collect)
  :commands org-mode
  :config

  
  (use-package org-agenda
    :demand t
    :config

    (let ((map org-agenda-mode-map))
      (bind-key "\C-n" 'next-line map)
      (bind-key "\C-p" 'previous-line map)
      (bind-key "g" 'org-agenda-redo map)
      (bind-key "f" 'org-agenda-date-later map)
      (bind-key "b" 'org-agenda-date-earlier map)
      (bind-key "r" 'org-agenda-refile map)
      (bind-key "SPC" 'org-agenda-tree-to-indirect-buffer  map)
      (bind-key "F" 'org-agenda-follow-mode map)
      (bind-key "q" 'delete-window map)
      (bind-key "M-p" 'org-agenda-earlier map)
      (bind-key "M-n" 'org-agenda-later map)
      (bind-key ">" 'org-agenda-filter-by-top-headline map)

      (unbind-key "M-m" map))

    (add-hook 'org-agenda-mode-hook
              #'(lambda ()
                  (hl-line-mode 1)
                  (setq line-spacing 0.25))))
  

  (use-package org-smart-capture)
  (use-package org-velocity)
  (use-package org-bullets)
  (use-package ox-mediawiki)

  (use-package org-knowledgebase
    :config
    (push '("Collector.org" :position bottom :height 15 :stick t)
          popwin:special-display-config))


  ;; ====== Org-mode settings

  (defun jump-to-org-agenda ()
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
          (call-interactively 'org-agenda-list)))))

  
  (let ((map org-mode-map))
    (bind-key "C-M-<return>" 'org-insert-heading-after-current map)
    (bind-key "C-<return>" 'other-window map)
    (bind-key "<return>" 'org-return-indent map)
    (bind-key "C-c C-x @" 'visible-mode map)
    (bind-key "C-c c" 'org-cleanup map)
    (unbind-key "C-<tab>" map)
    (unbind-key "M-{" map)
    (unbind-key "M-}" map)
    (unbind-key "C-y" map))

  (add-hook 'org-mode-hook
            #'(lambda ()
                (setq line-spacing 0.25)
                (buffer-face-mode 1)
                (org-bullets-mode 1)
                (flyspell-mode 1)))
  
  (defun save-org-mode-files ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (if (and (buffer-modified-p) (buffer-file-name))
              (save-buffer))))))
  (run-with-idle-timer 25 t 'save-org-mode-files)

  (defadvice org-refile-get-location (around my-org-refile-target activate)
    "Refile entry to specific category such as Notes or Tasks"
    (let* ((it ad-do-it)
           (file (nth 1 it))
           (re (nth 2 it))
           (pos (nth 3 it))
           (target-headline (if (member (if (equal major-mode 'org-agenda-mode)
                                            "TODO"
                                          (org-get-todo-state))
                                        '("NOTE" ""))
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


  ;; ====== automatically clean
  (defun org-archive-done-tasks ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\* \\(DONE\\|CANCELED\\) " nil t)
        (if (save-restriction
              (save-excursion
                (org-narrow-to-subtree)
                (search-forward ":LOGBOOK:" nil t)))
            (forward-line)
          (org-archive-subtree)
          (goto-char (line-beginning-position))))))

  (defun org-sort-all ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\* " nil t)
        (goto-char (match-beginning 0))
        (condition-case err
            (progn
              (org-sort-entries t ?a)
              (org-sort-entries t ?p)
              (org-sort-entries t ?o))
          (error nil))
        (forward-line))
      (goto-char (point-min))
      (while (re-search-forward "\* PROJECT " nil t)
        (goto-char (line-beginning-position))
        (ignore-errors
          (org-sort-entries t ?a)
          (org-sort-entries t ?p)
          (org-sort-entries t ?o))
        (forward-line))))

  (defun org-cleanup ()
    (interactive)
    (org-archive-done-tasks)
    (org-sort-all)))


(provide 'org-init)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; org-init.el ends here
