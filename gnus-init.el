;;;_ , Gnus
;;; useful links: http://visayafan.com/coding/lisp/EmacsGnus.html

(eval-when-compile
  (require 'cl))
(require 'use-package)

(load "gnus-settings")

(require 'gnus)
(require 'starttls)
;; (require 'nnmairix)
(require 'message)
(require 'spam)
(require 'spam-report)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'bbdb-message)
(require 'async)
(require 'escreen)

;; ------ initialize
;; (gnus-compile)
(gnus-delay-initialize)
(spam-initialize)
(bbdb-initialize 'gnus 'message)

;; ------ BBDB
;; (add-hook 'message-mode-hook
;;           '(lambda ()
;;              (local-set-key "<TAB>" 'bbdb-complete-mail)))

;; ------ Gnus Trigger
(defvar m-gnus-unplugged nil)
(defvar m-gnus-running nil)
(defvar m-gnus-current-screen-number nil)
(defun trigger-gnus(&optional unplugged-p)
  (interactive "P")
  (if m-gnus-current-screen-number
      (if (equal m-gnus-current-screen-number
                 escreen-current-screen-number)
          (escreen-goto-last-screen)
        (escreen-goto-screen m-gnus-current-screen-number)
        (if (not m-gnus-running)
            (launch-gnus unplugged-p)
          (let ((gnus-buffer-regexp
                 "\\*\\(unsent\\|Summary\\|Group\\|Article\\)"))
            (unless (string-match-p gnus-buffer-regexp (buffer-name))
              (launch-gnus-buffer))
            (unless (string-match-p gnus-buffer-regexp (buffer-name))
              (launch-gnus unplugged-p)))))
    (progn
      (escreen-create-screen)
      (setq m-gnus-current-screen-number escreen-current-screen-number)
      (launch-gnus unplugged-p))))
(defun launch-gnus (&optional unplugged-p)
  (if (and (not unplugged-p) (quickping "www.baidu.com"))
      (gnus)
    (gnus-unplugged)
    (setq m-gnus-unplugged t))
  (gnus-group-list-groups gnus-activate-level)
  (setq m-gnus-running t))
(defun launch-gnus-buffer ()
  (let* ((alist '("\\`\\*unsent" "\\`\\*Summary" "\\`\\*Group"))
         (candidate
          (catch 'found
            (dolist (regexp alist)
              (dolist (buf (buffer-list))
                (if (string-match regexp (buffer-name buf))
                    (throw 'found buf)))))))
    (when candidate
      (if (featurep 'ido)
          (ido-visit-buffer candidate ido-default-buffer-method)
        (switch-to-buffer candidate))
      (if (string-match "Group" (buffer-name candidate))
          (gnus-group-get-new-news)))))

;; ------ fetchmail
(use-package fetchmail-ctl
  :commands switch-to-fetchmail
  :init
  (defun maybe-start-fetchmail-and-news ()
    (interactive)
    (unless m-gnus-unplugged
      (message "going to switch-to-fetchmail")
      (switch-to-fetchmail)))
  (add-hook 'gnus-startup-hook 'maybe-start-fetchmail-and-news))

(defun my-signature()
    "the individual signature"
    "Cheers
Benn Ma
bennmsg@gmail.com
benn@thenetcircle.com")

;; ------ separator

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(defun my-message-header-setup-hook ()
  (message-remove-header "From")
  (let ((gcc (message-field-value "Gcc")))
   (when (or (null gcc)
             (string-match "nnfolder\\+archive:" gcc))
     (message-remove-header "Gcc")
     (message-add-header
      (format "Gcc: %s" "mail.sent")))))
(add-hook 'message-header-setup-hook 'my-message-header-setup-hook)
(defadvice gnus-summary-resend-message-edit (after call-my-mhs-hook activate)
  (my-message-header-setup-hook))

(defun my-gnus-summary-save-parts (&optional arg)
  (interactive "P")
  (let ((directory "~/Downloads"))
    (message "Saving all MIME parts to %s..." directory)
    (gnus-summary-save-parts ".*" directory arg)
    (message "Saving all MIME parts to %s...done" directory)))
(bind-key "X m" 'my-gnus-summary-save-parts gnus-summary-mode-map)

(eval-when-compile
  (defvar gnus-agent-queue-mail))

(defun queue-message-if-not-connected ()
  (set (make-local-variable 'gnus-agent-queue-mail)
       (if (quickping "www.baidu.com") t 'always)))
(add-hook 'message-send-hook 'queue-message-if-not-connected)

(defun my-message-sender-setting ()
  (let ((from (message-field-value "From")))
    (make-local-variable 'message-sendmail-extra-arguments)
    (make-local-variable 'user-mail-address)
    (cond
     ((string-match-p "@163\\.com" from)
      (setq message-sendmail-extra-arguments (append message-sendmail-extra-arguments '("-a" "163"))
            user-mail-address from))
     ((string-match-p "@thenetcircle\\.com" from)
      (setq message-sendmail-extra-arguments (append message-sendmail-extra-arguments '("-a" "tnc"))
            user-mail-address from))
     ((string-match-p "@gmail\.com" from)
      (setq message-sendmail-extra-arguments (append message-sendmail-extra-arguments '("-a" "gmail"))
            user-mail-address from))
     (t
      (setq message-sendmail-extra-arguments (append message-sendmail-extra-arguments '("-a" "163"))
            user-mail-address "sjembn@163.com")))
    (message "msmtp send init: %s - %s" message-sendmail-extra-arguments user-mail-address)))
(add-hook 'message-send-hook 'my-message-sender-setting)

(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

;; Convert the send time to a local time
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)

;; ------ separator

(if window-system
    (setq
     gnus-summary-same-subject ""
     gnus-sum-thread-tree-indent " "
     gnus-sum-thread-tree-single-indent "☆ "
     gnus-sum-thread-tree-root "● "
     gnus-sum-thread-tree-false-root "◎ "
     gnus-sum-thread-tree-vertical "│"
     gnus-sum-thread-tree-leaf-with-other "├─► "
     gnus-sum-thread-tree-single-leaf "╰─► "))

(defsubst my-gnus-tos (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))
(defun gnus-user-format-function-S (header)
  "Return how much time it's been since something was sent."
  (condition-case err
      (let ((date (mail-header-date header)))
        (if (> (length date) 0)
            (let*
                ((then (my-gnus-tos
                        (apply 'encode-time (parse-time-string date))))
                 (now (my-gnus-tos (current-time)))
                 (diff (- now then))
                 (str
                  (cond
                   ((>= diff (* 86400.0 7.0 52.0))
                    (if (>= diff (* 86400.0 7.0 52.0 10.0))
                        (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                      (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                   ((>= diff (* 86400.0 30.0))
                    (if (>= diff (* 86400.0 30.0 10.0))
                        (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                      (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                   ((>= diff (* 86400.0 7.0))
                    (if (>= diff (* 86400.0 7.0 10.0))
                        (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                      (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                   ((>= diff 86400.0)
                    (if (>= diff (* 86400.0 10.0))
                        (format "%3dd" (floor (/ diff 86400.0)))
                      (format "%3.1fd" (/ diff 86400.0))))
                   ((>= diff 3600.0)
                    (if (>= diff (* 3600.0 10.0))
                        (format "%3dh" (floor (/ diff 3600.0)))
                      (format "%3.1fh" (/ diff 3600.0))))
                   ((>= diff 60.0)
                    (if (>= diff (* 60.0 10.0))
                        (format "%3dm" (floor (/ diff 60.0)))
                      (format "%3.1fm" (/ diff 60.0))))
                   (t
                    (format "%3ds" (floor diff)))))
                 (stripped
                  (replace-regexp-in-string "\\.0" "" str)))
              (concat (cond
                       ((= 2 (length stripped)) "  ")
                       ((= 3 (length stripped)) " ")
                       (t ""))
                      stripped))))
    (error "    ")))

(defvar gnus-count-recipients-threshold 5
  "*Number of recipients to consider as large.")

(defun gnus-user-format-function-r (header)
  "Given a Gnus message header, returns priority mark.
Here are the meanings:

The first column represent my relationship to the To: field.  It can be:

         I didn't appear (and the letter had one recipient)
   :     I didn't appear (and the letter had more than one recipient)
   <     I was the sole recipient
   +     I was among a few recipients
   *     There were many recipients

The second column represents the Cc: field:

    .    I wasn't mentioned, but one other was
    :    I wasn't mentioned, but others were
    ^    I was the only Cc mentioned
    &    I was among a few Cc recipients
    %    I was among many Cc recipients

These can combine in some ways to tell you at a glance how visible the message
is:

   >.    Someone wrote to me and one other
    &    I was copied along with several other people
   *:    Mail to lots of people in both the To and Cc!"
  (let* ((to (or (cdr (assoc 'To (mail-header-extra header))) ""))
         (cc (or (cdr (assoc 'Cc (mail-header-extra header))) ""))
         (to-len (length (split-string to "\\s-*,\\s-*")))
         (cc-len (length (split-string cc "\\s-*,\\s-*")))
         (to-char (cond )))
    (cond ((string-match gnus-ignored-from-addresses to)
           (cond ((= to-len 1)
                  (cond ((string= cc "") "< ")
                        ((= cc-len 1) "<.")
                        (t "<:")))
                 ((< to-len gnus-count-recipients-threshold)
                  (cond ((string= cc "") "+ ")
                        ((= cc-len 1) "+.")
                        (t "+:")))
                 (t
                  (cond ((string= cc "") "* ")
                        ((= cc-len 1) "*.")
                        (t "*:")))))

          ((string-match gnus-ignored-from-addresses cc)
           (cond ((= cc-len 1)
                  (cond ((= to-len 1) " ^")
                        (t ":^")))
                 ((< cc-len gnus-count-recipients-threshold)
                  (cond ((= to-len 1) " &")
                        (t ":&")))
                 (t
                  (cond ((= to-len 1) " %")
                        (t ":%")))))
          (t "  "))))

;; ------ separator

(use-package message-x)

(use-package gnus-dired
  :commands gnus-dired-mode
  :init
  (add-hook 'dired-mode-hook 'gnus-dired-mode))

;; (use-package my-gnus-score
;;   :commands (my-gnus-score-groups my-gnus-score-followup)
;;   :init
;;   (defun gnus-group-get-all-new-news ()
;;     (interactive)
;;     (gnus-group-get-new-news 5)
;;     (gnus-group-list-groups 4)
;;     (my-gnus-score-groups)
;;     (gnus-group-list-groups 4))

;;   (define-key gnus-group-mode-map [?v ?g] 'gnus-group-get-all-new-news))

(use-package gnus-demon
  :config
  (gnus-demon-add-handler 'gnus-demon-scan-news 120 10))

(use-package gnus-desktop-notify
  :config
  (defun my-gnus-desktop-notify (groups)
    "override build-in gnus-desktop-notify-exec"
    (let ((groups (mapcar 'gnus-desktop-notify-arg groups))
          (title "You have new news:")
          (command "(trigger-gnus)"))
      (case gnus-desktop-notify-behavior
        ('gnus-desktop-notify-single
         (dolist (g groups)
           (my-notify (shell-quote-argument g) command title "gnus") ))
        ('gnus-desktop-notify-multi
         (my-notify (mapconcat 'shell-quote-argument groups "\n") command title "gnus") ))))
  (setq gnus-desktop-notify-function 'my-gnus-desktop-notify)
  (gnus-desktop-notify-mode))

(defun activate-gnus ()
  (unless (get-buffer "*Group*") (gnus)))

(use-package nnir
  :init
  (defvar gnus-query-history nil)

  (defun gnus-query (query &optional arg)
    (interactive
     (list (read-string (format "IMAP Query %s: "
                                (if current-prefix-arg "All" "Mail"))
                        (format-time-string "SENTSINCE %d-%b-%Y "
                                            (time-subtract (current-time)
                                                           (days-to-time 90)))
                        'gnus-query-history)
           current-prefix-arg))
    (activate-gnus)
    (let ((nnir-imap-default-search-key "imap")
          (nnir-ignored-newsgroups
           (if arg
               (concat (regexp-opt
                        '("archive"
                          "archive.emacs"
                          "list"
                          "list.bahai"
                          "list.boost"
                          "list.clang"
                          "list.emacs"
                          "list.isocpp"
                          "list.ledger"
                          "list.llvm"
                          "list.wg21"
                          "mail"
                          "mail.save"
                          "Drafts"
                          "Sent Messages"))
                       "\\'")
             (concat "\\(\\(list\\|archive\\)\\.\\|"
                     "mail\\.\\(spam\\|save\\|trash\\|sent\\)\\)"))))
      (gnus-group-make-nnir-group
       nil (list (cons 'nnir-query-spec
                       (list (cons 'query query)
                             (cons 'criteria "")))
                 (cons 'nnir-group-spec
                       (list (list "nnimap:Local")))))))

  (define-key global-map [(alt meta ?f)] 'gnus-query))

(use-package gnus-harvest
  :commands gnus-harvest-install
  :demand t
  :config
  (if (featurep 'message-x)
      (gnus-harvest-install 'message-x)
    (gnus-harvest-install)))

(use-package gnus-alias
  :commands (gnus-alias-determine-identity
             gnus-alias-message-x-completion
             gnus-alias-select-identity)
  :init
  (add-hook 'message-setup-hook 'gnus-alias-determine-identity)

  (if (featurep 'message-x)
      (add-hook 'message-x-after-completion-functions
                'gnus-alias-message-x-completion))

  (define-key message-mode-map "\C-c\C-f\C-p" 'gnus-alias-select-identity))

(eval-when-compile
  (defvar gnus-balloon-face-0)
  (defvar gnus-balloon-face-1))

(use-package rs-gnus-summary
  :config
  (defalias 'gnus-user-format-function-size
    'rs-gnus-summary-line-message-size)

  (setq gnus-balloon-face-0 'rs-gnus-balloon-0)
  (setq gnus-balloon-face-1 'rs-gnus-balloon-1))

(use-package supercite
  :commands sc-cite-original
  :init
  (progn
    (add-hook 'mail-citation-hook 'sc-cite-original)

    (defun sc-remove-existing-signature ()
      (save-excursion
        (goto-char (region-beginning))
        (when (re-search-forward message-signature-separator (region-end) t)
          (delete-region (match-beginning 0) (region-end)))))

    (add-hook 'mail-citation-hook 'sc-remove-existing-signature)
    )

  :config
  (defun sc-fill-if-different (&optional prefix)
    "Fill the region bounded by `sc-fill-begin' and point.
Only fill if optional PREFIX is different than
`sc-fill-line-prefix'.  If `sc-auto-fill-region-p' is nil, do not
fill region.  If PREFIX is not supplied, initialize fill
variables.  This is useful for a regi `begin' frame-entry."
    (if (not prefix)
        (setq sc-fill-line-prefix ""
              sc-fill-begin (line-beginning-position))
      (if (and sc-auto-fill-region-p
               (not (string= prefix sc-fill-line-prefix)))
          (let ((fill-prefix sc-fill-line-prefix))
            (unless (or (string= fill-prefix "")
                        (save-excursion
                          (goto-char sc-fill-begin)
                          (or (looking-at ">+  +")
                              (< (length
                                  (buffer-substring (point)
                                                    (line-end-position)))
                                 65))))
              (fill-region sc-fill-begin (line-beginning-position)))
            (setq sc-fill-line-prefix prefix
                  sc-fill-begin (line-beginning-position)))))
    nil))

;; ====== hacks
(defvar use-spam-filtering nil)
;; Override definition from spam.el to use async.el
(defun spam-spamassassin-register-with-sa-learn (articles spam
                                                          &optional unregister)
  "Register articles with spamassassin's sa-learn as spam or non-spam."
  (if (and use-spam-filtering articles)
      (let ((action (if unregister spam-sa-learn-unregister-switch
                      (if spam spam-sa-learn-spam-switch
                        spam-sa-learn-ham-switch)))
            (summary-buffer-name (buffer-name)))
        (with-temp-buffer
          ;; group the articles into mbox format
          (dolist (article articles)
            (let (article-string)
              (with-current-buffer summary-buffer-name
                (setq article-string (spam-get-article-as-string article)))
              (when (stringp article-string)
                ;; mbox separator
                (insert (concat "From nobody " (current-time-string) "\n"))
                (insert article-string)
                (insert "\n"))))
          ;; call sa-learn on all messages at the same time, and also report
          ;; them as SPAM to the Internet
          (async-start
           `(lambda ()
              (with-temp-buffer
                (insert ,(buffer-substring-no-properties
                          (point-min) (point-max)))
                (call-process-region (point-min) (point-max)
                                     ,spam-sa-learn-program
                                     nil nil nil "--mbox"
                                     ,@(if spam-sa-learn-rebuild
                                           (list action)
                                         (list "--no-rebuild" action)))
                (if ,spam
                    (call-process-region (point-min) (point-max)
                                         ,(executable-find "spamassassin-5.12")
                                         nil nil nil "--mbox" "-r"))))
           `(lambda (&optional ignore)
              (message  "Finished learning messsages as %s"
                        ,(if spam "spam" "ham"))))))))

(provide 'gnus-init)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; dot-gnus.el ends here