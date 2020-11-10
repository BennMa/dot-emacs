;; (run-with-idle-timer 10 nil 'mu4e) 

(general-define-key "C-c m" 'hydra-mail/body
                    "C-c M" '(lambda() (interactive) (error (switch-to-buffer "*mu4e-headers*")))
                    "C-x m" 'mu4e-compose-new)
(defhydra hydra-mail (:hint nil :color blue :columns 4)
  "Mail Helper"
  ("m" mu4e "Mu4e Main")
  ("j" mu4e~headers-jump-to-maildir "Jump to maildir")
  ("c" mu4e-compose-new "Compose new")
  ("b" mu4e-headers-search-bookmark "Bookmarks")
  ("q" nil "Cancel"))

(general-define-key :keymaps 'mu4e-main-mode-map
                    "q"     'my/last-buffer)
(general-define-key :keymaps 'mu4e-headers-mode-map
                    "q"     'my/last-buffer)

(defvar my-mu4e-dir (cond
                    ((eq system-type 'darwin) "/usr/local/Cellar/mu/0.9.18/share/emacs/site-lisp/mu/mu4e")
                    ((eq system-type 'gnu/linux) "/usr/local/share/emacs/site-lisp/mu4e")))

(use-package mu4e :ensure nil
  :load-path my-mu4e-dir
  :commands (mu4e
             mu4e~headers-jump-to-maildir
             mu4e-compose-new
             mu4e-headers-search-bookmark)
  :config
  (progn
    ;; tell mu4e where my Maildir is
    (setq mu4e-maildir (expand-file-name "~/Mail"))
    (setq mail-user-agent 'mu4e-user-agent)
    (setq mu4e-completing-read-function 'completing-read)

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-action-view-in-browser) t)
    
    ;; tell mu4e how to sync email
    (setq mu4e-mu-binary (executable-find "mu"))
    (setq mu4e-get-mail-command
          (concat
           (executable-find "mbsync") " benntnc" " && "
           (executable-find "proxychains") " " (executable-find "mbsync") " bennmsg"))

    ;; taken from mu4e page to define bookmarks
    (add-to-list 'mu4e-bookmarks
                 '("size:5M..500M"       "Big messages"     ?b))

    ;; ;; tell mu4e to use w3m for html rendering
    (let ((w3m (executable-find "w3m")))
      (setq mu4e-html2text-command
            (if w3m
                (format "%s -T text/html" w3m)
              (cond
               ((eq system-type 'darwin) "textutil -stdin -format html -convert txt -stdout")
               ((eq system-type 'gnu/linux) (format "html2text -utf8 -width %d" fill-column))))))

  ;; mu4e requires to specify drafts, sent, and trash dirs
  ;; a smarter configuration allows to select directories according to the account (see mu4e page)
  ;; (setq mu4e-drafts-folder "/work/drafts")
  ;; (setq mu4e-sent-folder "/work/sent")
  ;; (setq mu4e-trash-folder "/work/trash")

  (use-package mu4e-maildirs-extension
    :config (mu4e-maildirs-extension))

  (use-package org-mu4e :ensure nil
    :after org)
  ))


(use-package mu4e-alert
  :init (add-hook 'mu4e-main-mode-hook 'mu4e-alert-enable-notifications)
  ;; :init (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  :commands (mu4e-alert-enable-notifications)
  :config
  (progn
    (mu4e-alert-set-default-style
     (cond
      ((eq system-type 'darwin) 'notifier) ;; requires `terminal-notifier'
      ((eq system-type 'gnu/linux) 'libnotify)))))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
