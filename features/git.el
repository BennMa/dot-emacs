(general-define-key "C-c g" 'hydra-git/body)
(defhydra hydra-git (:hint nil :color blue :columns 4)
  "Magit Helper"
  ("s" magit-status "Status")
  ("S" magit-stage-file "Stage")
  ("c" magit-commit "Commit")
  ("C" magit-checkout "Checkout")
  ("r" magit-revert "Revert")
  ("d" magit-diff-unstaged "Diff unstaged")
  ("b" magit-blame "Blame")
  ("l" magit-log-buffer-file "Log of this file")
  ("t" hydra-git-timemachine/body "Toggle timemachine")
  ("m" git-messenger:popup-message "Commit Message")
  ("g" hydra-git-gutter/body "Gutter")
  ("u" git-link "Copy url")
  ("q" nil "Cancel"))

(defhydra hydra-git-timemachine (:body-pre (git-timemachine)
                                           :hint nil :color pink :columns 4)
  "Git Timemachine Helper"
  ("p" git-timemachine-show-previous-revision "Prev")
  ("n" git-timemachine-show-next-revision "Next")
  ("g" git-timemachine-show-nth-revision "Goto")
  ("w" git-timemachine-kill-abbreviated-revision "Copy short revision")
  ("W" git-timemachine-kill-revision "Copy revision")
  ("q" git-timemachine-quit "Cancel" :exit t))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil :color pink :columns 4)
  "Git Gutter Helper"
  ("g" git-gutter "Gutter")
  ("d" git-gutter:popup-hunk "Diff")
  ("p" git-gutter:previous-hunk "Prev")
  ("n" git-gutter:next-hunk "Next")
  ("<" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "First")
  (">" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "Last")
  ("s" git-gutter:stage-hunk "Stage")
  ("r" git-gutter:revert-hunk "Revert")
  ("q" nil "Cancel" :color blue))

(use-package magit
  :commands (magit-status
             magit-stage-file
             magit-revert
             magit-diff-unstaged
             magit-commit
             magit-blame
             magit-checkout
             magit-log-buffer-file)
  :config
  (progn
    (setenv "GIT_PAGER" "")
    (add-hook 'magit-mode-hook 'hl-line-mode)

    ;; (unbind-key "M-h" magit-mode-map)
    ;; (unbind-key "M-s" magit-mode-map)
    ;; (unbind-key "M-m" magit-mode-map)
    ;; (unbind-key "M-w" magit-mode-map)
    ;; (unbind-key "C-<return>" magit-file-section-map)

    ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
    ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
    (bind-key "U" #'magit-unstage-all magit-mode-map)

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    (defun magit-monitor (&optional no-display)
      "Start git-monitor in the current directory."
      (interactive)
      (when (string-match "\\*magit: \\(.+?\\)\\*" (buffer-name))
        (let ((name (format "*git-monitor: %s*"
                            (match-string 1 (buffer-name)))))
          (or (get-buffer name)
              (let ((buf (get-buffer-create name)))
                (ignore-errors
                  (start-process "*git-monitor*" buf "git-monitor"
                                 "-d" (expand-file-name default-directory)))
                buf)))))
    ;; (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

    (defun eshell/git (&rest args)
      (cond
       ((or (null args)
            (and (string= (car args) "status") (null (cdr args))))
        (magit-status-internal default-directory))
       ((and (string= (car args) "log") (null (cdr args)))
        (magit-log "HEAD"))
       (t (throw 'eshell-replace-command
                 (eshell-parse-command
                  "*git"
                  (eshell-stringify-list (eshell-flatten-list args)))))))))

(use-package magithub
  :disabled t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package git-timemachine
  :commands (git-timemachine
             git-timemachine-toggle))

(use-package git-messenger
  :commands (git-messenger:popup-message))

(use-package git-gutter
  :diminish "GT"
  :commands (git-gutter-mode)
  ;; :init (add-hook 'prog-mode-hook 'git-gutter-mode)
  :config
  (progn
    (git-gutter:linum-setup)))

(use-package git-link
  :commands git-link)
