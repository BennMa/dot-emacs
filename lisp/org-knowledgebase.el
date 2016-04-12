;;; Package -- Summary

;;; Commentary:

;;; Code:

(defcustom my-knowledagebase-dir "~/Dropbox/PKG/Document"
  "Personal Knowledge Base Directory"
  :type 'string)
(defcustom my-daily-dir "~/Dropbox/PKG/Task/Daily"
  "Personal Daily Directory"
  :type 'string)
(defcustom my-collector-file "~/Dropbox/PKG/Document/Collector.org"
  "Personal Small Piece Collector"
  :type 'string)

(setq org-agenda-files
      (append org-agenda-files
              (directory-files-recursively my-knowledagebase-dir t org-agenda-file-regexp)))

(defvar org-knowledgebase-map)
(define-prefix-command 'org-knowledgebase-map)
(bind-key "C-. k" 'org-knowledgebase-map)


;; ------ knowledgebase file list
(bind-key "l" 'k/list org-knowledgebase-map)
(setq k/list-history nil)
(defun k/list()
  "List all knowledge base files."
  (interactive)
  (helm :sources
        (helm-build-sync-source "Knowledge Files"
          :candidates (--map
                       (cons (substring it (1+ (length (expand-file-name my-knowledagebase-dir)))) it)
                       (directory-files-recursively my-knowledagebase-dir t "\\.org$"))
          :action (helm-make-actions
                   "Find File" 'helm-find-file-or-marked
                   "Find file in Dired" 'helm-point-file-in-dired
                   "Grep File(s) `C-s, C-u Recurse'" 'helm-find-files-grep))
        :buffer "*Knowledge Files*"
        :history 'k/list-history))

;; ------ daily file list
(defun archive-my-daily-jobs()
  ;;(when (string= org-state "DONE")
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file
          (expand-file-name (concat today ".org")
                            my-daily-dir)))
    (unless (file-exists-p daily-file)
      (with-current-buffer (find-file-noselect daily-file)
        (insert
         (format "Daily on %s    -*- mode: org; -*-\n#+STARTUP: overview\n" today))
        (save-buffer)))
    (org-refile 3 nil (list "Daily File" daily-file))))
(add-hook 'org-after-todo-state-change-hook 'archive-my-daily-jobs)
(add-hook 'org-clock-out-hook 'archive-my-daily-jobs)

(defun k/daily-list()
  "List all daily files."
  (interactive)
  (helm :sources
        (helm-build-sync-source "Daily Files"
          :candidates (--map
                       (cons (substring it (1+ (length (expand-file-name my-daily-dir)))) it)
                       (directory-files my-daily-dir t "\\.\\(txt\\|org\\)$" t))
          :action (helm-make-actions
                   "Find File" 'helm-find-file-or-marked
                   "Find file in Dired" 'helm-point-file-in-dired
                   "Grep File(s) `C-s, C-u Recurse'" 'helm-find-files-grep))
        :buffer "*Daily Files*"
        :history 'k/list-history))
(bind-key "d" 'k/daily-list org-knowledgebase-map)


;; ------ knowledge base search
(bind-key "s" 'k/search org-knowledgebase-map)
(defun k/search (&optional options)
  "Search knowledge base by helm-ag."
  (interactive)
  (if (require 'helm-grep nil  'noerror)
      (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg)))
        (helm-do-grep-1 (directory-files-recursively my-knowledagebase-dir t "\\.\\(?:org\\|html?\\)$")
                        prefarg))
    (error "helm-grep not available")))


;; ------ knowledge base review
(defvar k/review-amount-tag "M_REVIEWED_AMOUNT")
(defvar k/review-date-tag "M_REVIEWED_DATE")
(add-to-list 'org-agenda-custom-commands
             '("q" "All Review Entries" tags ":review:" 
               ((org-agenda-skip-function ;; (org-agenda-files (list my-knowledagebase-dir))
                 'k/org-agenda-skip-expired-review-entry))) t)
(defun k/org-agenda-skip-expired-review-entry()
  (let (beg end)
    (org-back-to-heading t)
    (setq beg (point)
          end (progn (outline-next-heading) (1- (point)))) ;; (progn (org-end-of-subtree t) (point))
    (goto-char beg)
    (and
     (let ((reviewed-amount (org-entry-get beg k/review-amount-tag))
           (reviewed-date   (org-entry-get beg k/review-date-tag))
           (closed          (org-entry-get beg "closed")))
       (not (k/org-review-forget-algorithm reviewed-amount
                                           (or reviewed-date closed))))
     end)))

(defun k/org-review-forget-algorithm(reviewed-amount reviewed-date)
  (if (and reviewed-amount reviewed-date)
      (let ((today (date-to-day (format-time-string "%Y-%m-%d 00:00:00")))
            (lastday (date-to-day reviewed-date))
            (_reviewed-amount (string-to-int reviewed-amount)))
        (cond
         ((= 1 _reviewed-amount) (>= (- today lastday) 7))
         ((= 2 _reviewed-amount) (>= (- today lastday) 30))
         ((= 3 _reviewed-amount) (>= (- today lastday) 90))
         ((> _reviewed-amount 3) (>= (- today lastday) 180))
         (t t)))
    t))

(org-defkey org-agenda-mode-map "D" 'k/org-agenda-magic-done)
(defun k/org-agenda-magic-done(&optional arg)
  "set current entry done review, and update statistics."
  (interactive "P")
  (let* ((col (current-column))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos    (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        ;; todo: do different action for different type of entry,  check this function org-agenda-skip-entry-if
        ;; review entry
        (let* ((reviewed-amount (org-entry-get (point) k/review-amount-tag))
               (reviewed-amount (int-to-string (if reviewed-amount
                                                   (1+ (string-to-int reviewed-amount))
                                                 1)))
               (reviewed-date   (concat "[" (format-time-string "%Y-%m-%d %a %H:%M") "]")))
          (org-entry-put (point) k/review-amount-tag reviewed-amount)
          (org-entry-put (point) k/review-date-tag   reviewed-date)))
      (org-agenda-redo t)
      (org-move-to-column col))))

;; ------ Collector

(defun org-collect()
  "collect stuff to collected file"
  (interactive)
  (let ((collect-file (expand-file-name my-collector-file))
        (string (if mark-active
                    (buffer-substring (region-beginning) (region-end))
                  ""))
        (pnt))
    (unless (string= (buffer-file-name) collect-file)
      (find-file-other-window collect-file))
    (goto-char (point-min))
    ;; (if (string= (buffer-substring-no-properties 1 4) "* \n")
    (if (looking-at-p "\\* +?\n")
        (goto-char 3)
      (progn
        (insert "\n")
        (goto-char (point-min))
        (insert "* ")))
    (save-excursion
      (unless (string= string "")
        (goto-char (if (search-forward-regexp "\n\\* .*\n" nil t)
                       (match-beginning 0)
                     (point)))
        (insert (concat "\n" string "\n"))))
    (save-buffer)))


(provide 'org-knowledgebase)

;;; org-knowledgebase.el ends here
