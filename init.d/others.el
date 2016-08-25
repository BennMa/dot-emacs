(use-package cus-edit :ensure nil)
(use-package initsplit :ensure nil)

(use-package nlinum
  ;; :disabled t
  :ensure t
  :config
  (global-nlinum-mode))

(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))



(use-package nf-procmail-mode
  :disabled t
  :commands nf-procmail-mode
  :mode ("procmailrc" . nf-procmail-mode))


(use-package w3m
  :commands (w3m w3m-browse-url
                 my-w3m-hacknews my-w3m-reddit my-w3m-wikipedia my-w3m-open-url)
  :config
  (defun my-w3m-hacknews ()
    (interactive)
    (browse-url "http://news.ycombinator.com"))
  (defun my-w3m-reddit (reddit)
    "Opens the REDDIT in w3m-new-session"
    (interactive (list
                  (read-string "Enter the reddit (default: php): " nil nil "php" nil)))
    (browse-url (format "http://m.reddit.com/r/%s" reddit)))
  (defun my-w3m-wikipedia (search-term)
    "Search for SEARCH-TERM on wikipedia"
    (interactive
     (let ((term (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (word-at-point))))
       (list
        (read-string
         (format "Wikipedia (%s):" term) nil nil term)))
     )
    (browse-url
     (concat
      "http://en.m.wikipedia.org/w/index.php?search="
      search-term
      )))
  (defun my-w3m-open-url (url)
    "Opens site in new w3m session with 'http://' appended"
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
    (w3m-goto-url-new-session
     (concat "http://" url))))

;; (use-package bbdb-com
;;   :commands bbdb-create
;;   :bind ("M-B" . bbdb))


;; terminal --------------------------------------------------------------------------




(use-package anzu
  :demand t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package minimap
  :disabled t
  :commands minimap)

(use-package esup
  :commands esup)

(use-package vertigo
  :bind (("M-P" . vertigo-jump-up)
         ("M-N" . vertigo-jump-down)))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package fill-column-indicator
  :disabled t
  :demand t  
  :commands fci-mode
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1))

;; https://github.com/pashky/restclient.el
(use-package restclient
  :commands restclient-mode)

(use-package symon
  :disabled t
  :config
  (symon-mode -1))

(use-package elscreen
  :config
  (elscreen-start))

;; https://github.com/rooney/zencoding
(use-package zencoding-mode
  :bind ("C-M-h" . zencoding-expand-line)
  :commands zencoding-mode
  :config
  (zencoding-mode)
  (unbind-key "C-j" zencoding-mode-keymap)
  (unbind-key "C-<return>" zencoding-mode-keymap))

;; ------ mongo shell
(use-package inf-mongo
  :commands inf-mongo
  :config
  (add-hook 'inf-mongo-mode-hook
            #'(lambda ()
                (projectile-mode -1)
                (company-mode -1)
                (auto-highlight-symbol-mode -1)
                (ansi-color-for-comint-mode-on))))

;; ------ erc
(use-package erc
  :disabled t
  :bind ("C-c C-r" . my-irc)
  :commands (erc erc-select)
  :config
  (defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
    "Create interactive command `command', for connecting to an IRC server. The
command uses interactive mode if passed an argument."
    (fset command
          `(lambda (arg)
             (interactive "p")
             (if (not (= 1 arg))
                 (call-interactively 'erc)
               (let ((erc-connect-function ',(if ssl
                                                 'erc-open-ssl-stream
                                               'open-network-stream)))
                 (erc :server ,server :port ,port :nick ,nick :password
                      ,pass))))))
  (defmacro erc-autojoin (&rest args)
    `(add-hook 'erc-after-connect
               '(lambda (server nick)
                  (cond
                   ,@(mapcar (lambda (servers+channels)
                               (let ((servers (car servers+channels))
                                     (channels (cdr servers+channels)))
                                 `((member erc-session-server ',servers)
                                   (mapc 'erc-join-channel ',channels))))
                             args)))))
  (load ".erc-auth") ;; this defined the erc-tnc
  (defun my-irc ()
    "Start to waste time on IRC with ERC."
    (interactive)
    (select-frame (make-frame '((name . "Emacs IRC")
                                (minibuffer . t))))
    (call-interactively 'erc-tnc)
    ;; (sit-for 1)
    )
  
  (defun erc-maybe-bol ()
    "Goto the end of `erc-prompt'.
 If already there, go to `beginning-of-line'."
    (interactive)
    (if (and (string-match (concat "^" (regexp-quote (erc-prompt))
                                   " *$")
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point)))
             (not (bolp)))
        (beginning-of-line)
      (erc-bol)))
  (bind-key "C-a" 'erc-maybe-bol erc-mode-map)
  
  (bind-key "C-m" 'newline erc-mode-map)
  (bind-key "C-c C-c" 'erc-send-current-line erc-mode-map)
  (bind-key "C-<return>" 'erc-send-current-line erc-mode-map)
  (bind-key  "C-c C-q"
             #'(lambda (nick)
                 (interactive (list (completing-read "Nick: " channel-members)))
                 (erc-cmd-QUERY nick))
             erc-mode-map)

  (defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
    "Insert last recipient after prompt."
    (let ((previous 
           (save-excursion 
             (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                      (search-forward-regexp (concat "^[^<]*<" erc-nick ">" 
                                                     " *\\([^:]*: ?\\)") nil t))
                 (match-string 1)))))
      ;; when we got something, and it was in the last 3 mins, put it in
      (when (and 
             previous 
             (> 180 (time-to-seconds 
                     (time-since (get-text-property 0 'timestamp previous)))))
        (set-text-properties 0 (length previous) nil previous)
        (insert previous)))))


;; https://github.com/jorgenschaefer/circe
;; https://github.com/jorgenschaefer/circe/wiki/Commands
(use-package circe
  :bind ("M-R" . my-toggle-circe)
  :config
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)

  (defun my-toggle-circe ()
    (interactive)
    (let* ((circe-mode-list '("circe-query-mode" "circe-server-mode" "circe-channel-mode" "circe-chat-mode"))
           (in-circe-p (member-ignore-case (symbol-name major-mode) circe-mode-list)))
      (if in-circe-p
          (call-interactively 'switch-to-previous-buffer)
        (let ((previous-buffer-white-list nil)
              (previous-buffer-white-modes-list circe-mode-list)
              (previous-buffer-black-list '(".*")))
          (or (call-interactively 'switch-to-previous-buffer)
              (circe "TNC")))))))

(use-package circe-notifications
  :commands (enable-circe-notifications)
  :init
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(use-package hiwin
  :disabled t
  :config
  (hiwin-activate)
  (set-face-background 'hiwin-face "gray18"))

;; ------ org-mode
;; (use-package org-init)

;; ------ gnus
;; (use-package gnus-init
;;   :disabled t
;;   :bind (("M-G"   . trigger-gnus)
;;          ("C-x m" . compose-mail)))
