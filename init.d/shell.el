(use-package multi-term
  :bind (("C-`" . my-term-toggle)
         ("M-t" . multi-term))
  :config
  (when (require 'term nil t)
    (defun term-handle-ansi-terminal-messages (message)
      (while (string-match "\eAnSiT.+\n" message)
        ;; Extract the command code and the argument.
        (let* ((start (match-beginning 0))
               (command-code (aref message (+ start 6)))
               (argument
                (save-match-data
                  (substring message
                             (+ start 8)
                             (string-match "\r?\n" message
                                           (+ start 8)))))
               ignore)
          ;; Delete this command from MESSAGE.
          (setq message (replace-match "" t t message))
          
          (cond ((= command-code ?c)
                 (setq term-ansi-at-dir argument))
                ((= command-code ?h)
                 (setq term-ansi-at-host argument))
                ((= command-code ?u)
                 (setq term-ansi-at-user argument))
                ((= command-code ?e)
                 (save-excursion
                   (find-file-other-window (expand-file-name argument))))
                ((= command-code ?x)
                 (save-excursion
                   (find-file argument)))
                ((= command-code ?g)
                 (save-excursion
                   (magit-status argument)))
                (t
                 (setq ignore t)))
          
          ;; (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
          ;;   (setq buffer-file-name
          ;;         (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
          ;;   (set-buffer-modified-p nil)
          ;;   (setq default-directory (if (string= term-ansi-at-host (system-name))
          ;;                               (concatenate 'string term-ansi-at-dir "/")
          ;;                             (format "/%s@%s:%s/" term-ansi-at-user
          ;;                                     term-ansi-at-host term-ansi-at-dir))))


          ;; (message "%s=%s=%s=%s=%s"
          ;;          term-ansi-at-host (system-name) term-ansi-at-user
          ;;          (user-real-login-name) term-ansi-at-dir)
          
          (if ignore
              nil
            (setq default-directory
                  (file-name-as-directory
                   (if (and (string= term-ansi-at-host "localhost") ;; (string= term-ansi-at-host (system-name))
                            (string= term-ansi-at-user (user-real-login-name)))
                       (expand-file-name term-ansi-at-dir)
                     (if (string= term-ansi-at-user (user-real-login-name))
                         (concat "/" term-ansi-at-host ":" term-ansi-at-dir)
                       (concat "/" term-ansi-at-user "@" term-ansi-at-host ":"
                               term-ansi-at-dir)))))

            ;; I'm not sure this is necessary,
            ;; but it's best to be on the safe side.
            (if (string= term-ansi-at-host (system-name))
                (progn
                  (setq ange-ftp-default-user term-ansi-at-save-user)
                  (setq ange-ftp-default-password term-ansi-at-save-pwd)
                  (setq ange-ftp-generate-anonymous-password term-ansi-at-save-anon))
              (setq term-ansi-at-save-user ange-ftp-default-user)
              (setq term-ansi-at-save-pwd ange-ftp-default-password)
              (setq term-ansi-at-save-anon ange-ftp-generate-anonymous-password)
              (setq ange-ftp-default-user nil)
              (setq ange-ftp-default-password nil)
              (setq ange-ftp-generate-anonymous-password nil)))

          ))
      message)

    (defun my-term-toggle ()
      "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
      (interactive)
      (let ((b (my-term-last-buffer (buffer-list))))
        (if (eq 'term-mode major-mode)
            (let ((previous-buffer-white-list
                   (remove "\\*terminal" previous-buffer-white-list)))
              (switch-to-previous-buffer))
          (if (not b)
              (multi-term)
            (switch-to-buffer b)))))

    (defun my-term-last-buffer (l)
      "Return most recently used term buffer."
      (when l
        (if (eq 'term-mode (with-current-buffer (car l) major-mode))
            (car l) (my-term-last-buffer (cdr l)))))

    ;; (defun my-term-clear ()
    ;;   (interactive)
    ;;   (let ((comint-buffer-maximum-size 0))
    ;;     (comint-truncate-buffer)))

    (add-hook 'term-mode-hook
              #'(lambda ()
                  (projectile-mode -1)
                  (company-mode -1)
                  (auto-highlight-symbol-mode -1)
                  (yas-minor-mode -1)))))


;; ------ Disabled

(use-package eshell
  :disabled t
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (defun ss (server)
      (interactive "sServer: ")
      (call-process "spawn" nil nil nil "ss" server))

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)

  (use-package esh-toggle
    :bind (("C-x C-z" . eshell-toggle)
           ("C-. C-z" . eshell-toggle-cd))))


(use-package sh-toggle
  :disabled t
  :bind ("C-. C-z" . shell-toggle))

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))
