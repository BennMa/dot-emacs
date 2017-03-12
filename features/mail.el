(use-package mu4e :ensure nil
  :load-path "/usr/local/Cellar/mu/0.9.18/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e)
  :config
  (progn
    ;; tell mu4e where my Maildir is
    (setq mu4e-maildir (expand-file-name "~/Mail"))
    (setq mail-user-agent 'mu4e-user-agent)
    
    ;; tell mu4e how to sync email
    (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))

    ;; taken from mu4e page to define bookmarks
    (add-to-list 'mu4e-bookmarks
                 '("size:5M..500M"       "Big messages"     ?b))

    ;; ;; tell mu4e to use w3m for html rendering
    ;; (setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")

    ;; mu4e requires to specify drafts, sent, and trash dirs
    ;; a smarter configuration allows to select directories according to the account (see mu4e page)
    ;; (setq mu4e-drafts-folder "/work/drafts")
    ;; (setq mu4e-sent-folder "/work/sent")
    ;; (setq mu4e-trash-folder "/work/trash")
  ))


(use-package mu4e-alert
  :disabled t
  :init (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  :config (mu4e-alert-set-default-style 'libnotify))
