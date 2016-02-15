;; ====== Org-mode

(require 'cl)
(require 'use-package)

(load "org-settings")

(require 'org)
(require 'org-agenda)
(require 'org-smart-capture)
(require 'org-crypt)
(require 'org-bbdb)
;; (require 'org-mac-link)
(require 'org-magit)
(require 'org-velocity)

(require 'ob-python)
(require 'ob-ruby)
(require 'ob-emacs-lisp)
(require 'ob-sh)

(require 'ox-md)
;; (require 'ox-opml)

(require 'calfw)
(require 'org-knowledgebase)

;; ====== Keybindings

;; ------ Org-global-keymap
(defvar org-mode-completion-keys
  '(
    (?t . "TODO")
    (?s . "STARTED")
    (?w . "WAITING")
    (?r . "DEFERRED")
    (?y . "SOMEDAY")
    (?x . "CANCELED")
    (?d . "DONE")
    (?n . "NOTE")
    ))

(defvar org-todo-state-map nil)
(define-prefix-command 'org-todo-state-map)

(dolist (ckey org-mode-completion-keys)
  (let* ((key (car ckey))
         (label (cdr ckey))
         (org-sym (intern (concat "my-org-todo-" (downcase label))))
         (org-sym-no-logging
          (intern (concat "my-org-todo-" (downcase label) "-no-logging")))
         (org-agenda-sym
          (intern (concat "my-org-agenda-todo-" (downcase label))))
         (org-agenda-sym-no-logging
          (intern (concat "my-org-agenda-todo-"
                          (downcase label) "-no-logging"))))
    (eval
     `(progn
        (defun ,org-sym ()
          (interactive)
          (org-todo ,label))
        (bind-key (concat "C-c x " (char-to-string ,key)) ',org-sym)

        (defun ,org-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-todo ,label)))
        (bind-key (concat "C-c x " (char-to-string  ,(upcase key)))
                  ',org-sym-no-logging)

        (defun ,org-agenda-sym ()
          (interactive)
          (let ((org-inhibit-logging
                 (let ((style (org-entry-get
                               (get-text-property (point) 'org-marker)
                               "STYLE")))
                   (and style (stringp style)
                        (string= style "habit")))))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,key] ',org-agenda-sym)

        (defun ,org-agenda-sym-no-logging ()
          (interactive)
          (let ((org-inhibit-logging t))
            (org-agenda-todo ,label)))
        (define-key org-todo-state-map [,(upcase key)]
          ',org-agenda-sym-no-logging)))))

(bind-key "C-c x m" 'org-insert-message-link)
(bind-key "C-c x M" 'org-set-message-link)
(bind-key "C-c x u" 'org-insert-url-link)
(bind-key "C-c x U" 'org-set-url-link)
(bind-key "C-c x f" 'org-insert-file-link)
(bind-key "C-c x F" 'org-set-file-link)

(org-defkey org-mode-map [(control meta return)]
            'org-insert-heading-after-current)
(org-defkey org-mode-map [(control return)] 'other-window)
(org-defkey org-mode-map [return] 'org-return-indent)
(org-defkey org-mode-map [(control ?c) (control ?x) ?@] 'visible-mode)
(org-defkey org-mode-map [(control ?c) (meta ?m)] 'my-org-wrap-region)

(remove-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

;; ------ Org-agenda-keymap

(let ((map org-agenda-mode-map))
  (define-key map "\C-n" 'next-line)
  (define-key map "\C-p" 'previous-line)

  (define-key map "g" 'org-agenda-redo)
  (define-key map "f" 'org-agenda-date-later)
  (define-key map "b" 'org-agenda-date-earlier)
  (define-key map "r" 'org-agenda-refile)
  (define-key map " " 'org-agenda-tree-to-indirect-buffer)
  (define-key map "F" 'org-agenda-follow-mode)
  (define-key map "q" 'delete-window)
  (define-key map [(meta ?p)] 'org-agenda-earlier)
  (define-key map [(meta ?n)] 'org-agenda-later)
  (define-key map "x" 'org-todo-state-map)

  (define-key map ">" 'org-agenda-filter-by-top-headline))

;; ------ unbind some conflicting keys

(unbind-key "C-<tab>" org-mode-map)
(unbind-key "M-m" org-agenda-keymap)
(unbind-key "M-{" org-mode-map)
(unbind-key "M-}" org-mode-map)


;; ====== Functions Define

(declare-function cfw:open-calendar-buffer "calfw")
(declare-function cfw:refresh-calendar-buffer "calfw")
(declare-function cfw:org-create-source "calfw-org")
(declare-function cfw:cal-create-source "calfw-cal")

(defun org-fit-agenda-window ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (fit-window-to-buffer)))

(defun my-org-startup ()
  (org-agenda-list)
  (org-fit-agenda-window)
  (org-agenda-to-appt)
  ;; (other-window 1)
  ;; (my-calendar)
  ;; (run-with-idle-timer
  ;;  0.1 nil
  ;;  (lambda ()
  ;;    (let ((wind (get-buffer-window "*Org Agenda*")))
  ;;      (when wind
  ;;        (set-frame-selected-window nil wind)
  ;;        (call-interactively #'org-agenda-redo)))
  ;;    (let ((wind (get-buffer-window "*cfw-calendar*")))
  ;;      (when wind
  ;;        (set-frame-selected-window nil wind)
  ;;        (call-interactively #'cfw:refresh-calendar-buffer)))
  ;;    (let ((wind (get-buffer-window "*Org Agenda*")))
  ;;      (when wind
  ;;        (set-frame-selected-window nil wind)
  ;;        (call-interactively #'org-resolve-clocks)))))
  )
(defun my-calendar ()
  (interactive)
  (let ((buf (get-buffer "*cfw-calendar*")))

    cfw:fchar-top-left-corner ?┏
    cfw:fchar-top-right-corner ?┓)

  (bind-key "j" 'cfw:navi-goto-date-command cfw:calendar-mode-map)
  (bind-key "g" 'cfw:refresh-calendar-buffer cfw:calendar-mode-map))


(defun jump-to-org-agenda ()
  (interactive)
  (let ((recordings-dir "~/Dropbox/Apps/Dropvox"))
    (ignore-errors
      (if (directory-files recordings-dir nil "\\`[^.]")
          (find-file recordings-dir))))
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (when (called-interactively-p 'any)
              (select-window wind)
              (org-fit-window-to-buffer))
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (progn
        (call-interactively 'org-agenda-list)
        (org-fit-agenda-window)))))


(defun org-get-global-property (name)
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (concat "#\\+PROPERTY: " name " \\(.*\\)") nil t)
         (match-string 1))))


(defun save-org-mode-files ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'org-mode)
        (if (and (buffer-modified-p) (buffer-file-name))
            (save-buffer))))))

(run-with-idle-timer 25 t 'save-org-mode-files)

(eval-when-compile
  (defvar org-clock-current-task))


;; ------ separator

(defun org-my-state-after-clock-out (state)
  (if (string= state "STARTED") "TODO" state))

;; ------ separator

(defvar org-my-archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be 0 for immediate, or a floating point value.")

(defconst org-my-ts-regexp
  "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]>\r\n]*?\\)[]>]"
  "Regular expression for fast inactive time stamp matching.")

(defun org-my-closing-time ()
  (let* ((state-regexp
          (concat "- State \"\\(?:" (regexp-opt org-done-keywords)
                  "\\)\"\\s-*\\[\\([^]\n]+\\)\\]"))
         (regexp (concat "\\(" state-regexp "\\|" org-my-ts-regexp "\\)"))
         (end (save-excursion
                (outline-next-heading)
                (point)))
         begin
         end-time)
    (goto-char (line-beginning-position))
    (while (re-search-forward regexp end t)
      (let ((moment (org-parse-time-string (match-string 1))))
        (if (or (not end-time)
                (time-less-p (apply #'encode-time end-time)
                             (apply #'encode-time moment)))
            (setq end-time moment))))
    (goto-char end)
    end-time))

(defun org-archive-expired-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done-regexp
           (concat "^\\*\\* \\(" (regexp-opt org-done-keywords) "\\) ")))
      (while (re-search-forward done-regexp nil t)
        (if (>= (time-to-number-of-days
                 (time-subtract (current-time)
                                (apply #'encode-time (org-my-closing-time))))
                org-my-archive-expiry-days)
            (org-archive-subtree))))
    (save-buffer)))

(defalias 'archive-expired-tasks 'org-archive-expired-tasks)

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

(defalias 'archive-done-tasks 'org-archive-done-tasks)

(defun org-get-inactive-time ()
  (float-time (org-time-string-to-time
               (or (org-entry-get (point) "TIMESTAMP")
                   (org-entry-get (point) "TIMESTAMP_IA")
                   (debug)))))

(defun org-get-completed-time ()
  (let ((begin (point)))
    (save-excursion
      (outline-next-heading)
      (and (re-search-backward
            (concat "\\(- State \"\\(DONE\\|DEFERRED\\|CANCELED\\)\""
                    "\\s-+\\[\\(.+?\\)\\]\\|CLOSED: \\[\\(.+?\\)\\]\\)")
            begin t)
           (float-time (org-time-string-to-time (or (match-string 3)
                                                    (match-string 4))))))))

(defun org-sort-done-tasks ()
  (interactive)
  (goto-char (point-min))
  (org-sort-entries t ?F #'org-get-inactive-time #'<)
  (goto-char (point-min))
  (while (re-search-forward "


+" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert "
"))
  (let (after-save-hook)
    (save-buffer))
  (org-overview))

(defalias 'sort-done-tasks 'org-sort-done-tasks)

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
  (org-archive-expired-tasks)
  (org-sort-all))


;; ------ separator

(defvar my-org-wrap-region-history nil)

(defun my-org-wrap-region (&optional arg)
  (interactive "P")
  (save-excursion
    (goto-char (region-end))
    (if arg
        (insert "#+end_src\n")
      (insert ":END:\n"))
    (goto-char (region-beginning))
    (if arg
        (insert "#+begin_src "
                (read-string "Language: " nil 'my-org-wrap-region-history)
                ?\n)
      (insert ":OUTPUT:\n"))))

;; ------ separator

(defun org-get-message-link (&optional title)
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (setq message-id (substring (message-field-value "message-id") 1 -1)
            subject (or title (message-field-value "subject"))))
    (org-make-link-string (concat "message://" message-id)
                          (rfc2047-decode-string subject))))

(defun org-insert-message-link (&optional arg)
  (interactive "P")
  (insert (org-get-message-link (if arg "writes"))))

(defun org-set-message-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Message" (org-get-message-link)))

(defun org-get-message-sender ()
  (let (message-id subject)
    (with-current-buffer gnus-original-article-buffer
      (message-field-value "from"))))

(defun org-set-message-sender ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Submitter" (org-get-message-sender)))

(defun org-get-safari-link ()
  (let ((subject (substring (do-applescript
                             (string-to-multibyte "tell application \"Safari\"
        name of document of front window
end tell")) 1 -1))
        (url (substring (do-applescript
                         (string-to-multibyte "tell application \"Safari\"
        URL of document of front window
end tell")) 1 -1)))
    (org-make-link-string url subject)))

(defun org-get-chrome-link ()
  (let ((subject (do-applescript
                  (string-to-multibyte "tell application \"Google Chrome\"
        title of active tab of front window
end tell")))
        (url (do-applescript
              (string-to-multibyte "tell application \"Google Chrome\"
        URL of active tab of front window
end tell"))))
    (org-make-link-string url subject)))

(defun org-insert-url-link ()
  (interactive)
  (insert (org-get-chrome-link)))

(defun org-set-url-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "URL" (org-get-chrome-link)))

(defun org-get-file-link ()
  (let* ((subject (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     name of beginning of theItems
end tell"))
         (path (do-applescript "tell application \"Path Finder\"
     set theItems to the selection
     (POSIX path of beginning of theItems) as text
end tell"))
         (short-path
          (replace-regexp-in-string abbreviated-home-dir "~/"
                                    (substring path 1 -1))))
    (org-make-link-string (concat "file:" short-path)
                          (substring subject 1 -1))))

(defun org-insert-file-link ()
  (interactive)
  (insert (org-get-file-link)))

(defun org-set-file-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "File" (org-get-file-link)))

(defun org-set-dtp-link ()
  "Set a property for the current headline."
  (interactive)
  (org-set-property "Document" (org-get-dtp-link)))

(defun org-dtp-message-open ()
  "Visit the message with the given MESSAGE-ID.
This will use the command `open' with the message URL."
  (interactive)
  (re-search-backward "\\[\\[message://\\(.+?\\)\\]\\[")
  (do-applescript
   (format "tell application \"DEVONthink Pro\"
        set searchResults to search \"%%3C%s%%3E\" within URLs
        open window for record (get beginning of searchResults)
end tell" (match-string 1))))

(add-hook 'org-log-buffer-setup-hook
          (lambda ()
            (setq fill-column (- fill-column 5))))

(defun org-message-reply ()
  (interactive)
  (let* ((org-marker (get-text-property (point) 'org-marker))
         (author (org-entry-get (or org-marker (point)) "Author"))
         (subject (if org-marker
                      (with-current-buffer (marker-buffer org-marker)
                        (goto-char org-marker)
                        (nth 4 (org-heading-components)))
                    (nth 4 (org-heading-components)))))
    (setq subject (replace-regexp-in-string "\\`(.*?) " "" subject))
    (compose-mail-other-window author (concat "Re: " subject))))


;; ====== Advice Define

;; (defadvice org-refile-get-location (before clear-refile-history activate)
;;   "Fit the Org Agenda to its buffer."
;;   (setq org-refile-history nil))
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
    (when (string-match-p "PROJECTS.txt$" file)
      (save-excursion
        (with-current-buffer (find-file-noselect file)
          (goto-char pos)
          (re-search-forward (concat "^\\*\\* " target-headline))
          (setcar (nthcdr 3 it) (point)))))
    it))

(defadvice org-agenda-redo (after fit-windows-for-agenda-redo activate)
  "Fit the Org Agenda to its buffer."
  (org-fit-agenda-window))

(defadvice org-agenda (around fit-windows-for-agenda activate)
  "Fit the Org Agenda to its buffer."
  ad-do-it
  (org-fit-agenda-window))

;; ====== collect
(defcustom my-collector-file "~/Dropbox/PKG/Document/Collector.org"
  "Personal Small Piece Collector"
  :type 'string)
(bind-key* "C-t" 'org-collect)
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


;; ====== hooks
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook #'(lambda ()
                             (flycheck-mode 1)))

(provide 'org-init)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;; ====== org-init.el ends here
