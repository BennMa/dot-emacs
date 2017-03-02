(general-define-key "<f3>" 'my-term-toggle
                    "S-<f3>" 'multi-term)

;; ------ packages
(use-package multi-term
  :commands (my-term-toggle
             multi-term)
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
            (let ((blaine--buffername-whitelist
                   (remove "\\*terminal" blaine--buffername-whitelist)))
              (blaine/last-buffer))
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
                  ;; (projectile-mode -1)
                  (company-mode -1)
                  ;; (auto-highlight-symbol-mode -1)
                  (yas-minor-mode -1)))))

;;; term.el ends here
