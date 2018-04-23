;;; org-knowledgebase.el --- my knowledge base -*- lexical-binding: t -*-

;; Copyright (C) 2016-02-01 baineng
;; License: MIT

;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'ivy)

(defcustom org-kb-doc-dir "~/Dropbox/PKG/Document"
  "Personal Knowledge Base Directory"
  :type 'string
  :group 'org-knowledgebase)
(defcustom org-kb-daily-dir "~/Dropbox/PKG/Task/Daily"
  "Personal Daily Directory"
  :type 'string
  :group 'org-knowledgebase)
(defcustom org-kb-collector-file "~/Dropbox/PKG/Task/Collector.org"
  "Personal Small Piece Collector"
  :type 'string
  :group 'org-knowledgebase)
(defvar org-kb--history nil)

(defun org-kb/show-all()
  "List all knowledge base files."
  (interactive)
  ;; (helm :sources
  ;;       (helm-build-sync-source "Knowledge Files"
  ;;         :candidates (--map
  ;;                      (cons (substring it (1+ (length (expand-file-name org-kb-doc-dir)))) it)
  ;;                      (directory-files-recursively org-kb-doc-dir t "\\.org$"))
  ;;         :action (helm-make-actions
  ;;                  "Find File" 'helm-find-file-or-marked
  ;;                  "Find file in Dired" 'helm-point-file-in-dired
  ;;                  "Grep File(s) `C-s, C-u Recurse'" 'helm-find-files-grep))
  ;;       :buffer "*Knowledge Files*"
  ;;       :history 'org-kb--history)
  (ivy-read "Knowledge Files: "
            (--map (cons (substring it (1+ (length (expand-file-name org-kb-doc-dir)))) it)
                   (directory-files-recursively org-kb-doc-dir t "\\.org$"))
            :action (lambda (x) (find-file (cdr x)))
            :history 'org-kb--history)
  )

;; ------ daily file list
(defun org-kb/archive-my-daily-jobs()
  ;;(when (string= org-state "DONE")
  (let* ((today (format-time-string "%Y-%m-%d"))
         (daily-file
          (expand-file-name (concat today ".org")
                            org-kb-daily-dir)))
    (unless (file-exists-p daily-file)
      (with-current-buffer (find-file-noselect daily-file)
        (insert
         (format "Daily on %s    -*- mode: org; -*-\n#+STARTUP: overview\n" today))
        (save-buffer)))
    (org-refile 3 nil (list "Daily File" daily-file))))

(defun org-kb/show-daily()
  "List all daily files."
  (interactive)
  ;; (helm :sources
  ;;       (helm-build-sync-source "Daily Files"
  ;;         :candidates (--map
  ;;                      (cons (substring it (1+ (length (expand-file-name org-kb-daily-dir)))) it)
  ;;                      (directory-files org-kb-daily-dir t "\\.\\(txt\\|org\\)$" t))
  ;;         :action (helm-make-actions
  ;;                  "Find File" 'helm-find-file-or-marked
  ;;                  "Find file in Dired" 'helm-point-file-in-dired
  ;;                  "Grep File(s) `C-s, C-u Recurse'" 'helm-find-files-grep))
  ;;       :buffer "*Daily Files*"
  ;;       :history 'org-kb--history)
  (ivy-read "Daily Files: "
            (--map (cons (substring it (1+ (length (expand-file-name org-kb-daily-dir)))) it)
                   (directory-files org-kb-daily-dir t "\\.\\(txt\\|org\\)$" t))
            :action (lambda (x) (find-file (cdr x)))
            :history 'org-kb--history)
  )

;; ------ knowledge base search
(defun org-kb/search ()
  "Search knowledge base by helm-ag."
  (interactive)
  ;; (if (require 'helm-grep nil  'noerror)
  ;;     (let* ((prefarg (or current-prefix-arg helm-current-prefix-arg)))
  ;;       (helm-do-grep-1 (directory-files-recursively org-kb-doc-dir t "\\.\\(?:org\\|html?\\)$")
  ;;                       prefarg))
  ;;   (error "helm-grep not available"))
  (counsel-ag "" (concat (expand-file-name org-kb-doc-dir) "/") nil "Knowledge Search"))


;; ------ knowledge base review
(defvar org-kb--review-amount-tag "M_REVIEWED_AMOUNT")
(defvar org-kb--review-date-tag "M_REVIEWED_DATE")
(defun org-kb/org-agenda-skip-expired-review-entry()
  (let (beg end)
    (org-back-to-heading t)
    (setq beg (point)
          end (progn (outline-next-heading) (1- (point)))) ;; (progn (org-end-of-subtree t) (point))
    (goto-char beg)
    (and
     (let ((reviewed-amount (org-entry-get beg org-kb--review-amount-tag))
           (reviewed-date   (org-entry-get beg org-kb--review-date-tag))
           (closed          (org-entry-get beg "closed")))
       (not (org-kb//org-review-forget-algorithm reviewed-amount
                                           (or reviewed-date closed))))
     end)))

(defun org-kb//org-review-forget-algorithm(reviewed-amount reviewed-date)
  (if (and reviewed-amount reviewed-date)
      (let ((today (date-to-day (format-time-string "%Y-%m-%d 00:00:00")))
            (lastday (date-to-day reviewed-date))
            (_reviewed-amount (string-to-number reviewed-amount)))
        (cond
         ((= 1 _reviewed-amount) (>= (- today lastday) 7))
         ((= 2 _reviewed-amount) (>= (- today lastday) 30))
         ((= 3 _reviewed-amount) (>= (- today lastday) 90))
         ((> _reviewed-amount 3) (>= (- today lastday) 180))
         (t t)))
    t))

(defun org-kb/org-agenda-magic-done(&optional arg)
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
        (let* ((reviewed-amount (org-entry-get (point) org-kb--review-amount-tag))
               (reviewed-amount (int-to-string (if reviewed-amount
                                                   (1+ (string-to-number reviewed-amount))
                                                 1)))
               (reviewed-date   (concat "[" (format-time-string "%Y-%m-%d %a %H:%M") "]")))
          (org-entry-put (point) org-kb--review-amount-tag reviewed-amount)
          (org-entry-put (point) org-kb--review-date-tag   reviewed-date)))
      (org-agenda-redo t)
      (org-move-to-column col))))

;; ------ Collector
(defun org-kb/collect()
  "collect stuff to collected file"
  (interactive)
  (let ((collect-file (expand-file-name org-kb-collector-file))
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
