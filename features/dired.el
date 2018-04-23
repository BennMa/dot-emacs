(general-define-key "C-x C-j" 'dired-jump
                    "C-x C-S-j" 'my-direx:jump-to-directory-other-window)

(use-package dired :ensure nil
  :config
  (progn
    (use-package dired-k
      :commands (dired-k direx-k dired-k-no-revert)
      :init
      (progn
        ;; always execute dired-k when dired buffer is opened
        (add-hook 'dired-initial-position-hook 'dired-k)
        (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))
    ))

(use-package direx
  :commands (my-direx:jump-to-directory-other-window)
  :config
  (progn
    (add-hook 'direx:direx-mode-hook
              #'(lambda ()
                  (linum-mode -1)))
    ;; (add-hook 'direx:direx-mode-hook 'direx-k)
    ;; (push '(direx:direx-mode :position left :width 30 :dedicated t :stick t)
    ;;       popwin:special-display-config)
    (push '(direx:direx-mode :align left :select t :size 30) shackle-rules)
    :config
    (defun my-direx:jump-to-directory-other-window ()
      (interactive)
      ;; (switch-to-buffer-other-window (direx:jump-to-directory-noselect))
      (direx:jump-to-directory-other-window)
      (set-window-dedicated-p (selected-window) t))

    (defadvice direx:jump-to-directory-noselect
        (around direx:set-default-directory activate)
      (let ((default-directory (projectile-project-root)))
        ad-do-it))

    (defun direx:do-rename-file ()
      (interactive)
      (let* ((item (direx:item-at-point!))
             (file (direx:item-tree item))
             (to (read-file-name (format "Rename %s to " (direx:tree-name file))
                                 (direx:directory-dirname (direx:file-full-name file)))))
        (dired-rename-file (direx:file-full-name file) to nil)
        (direx:item-refresh-parent item)
        (direx:move-to-item-name-part item)))

    (defun direx:do-copy-files ()
      (interactive)
      (let* ((item (direx:item-at-point!))
             (file (direx:item-tree item))
             (to (read-directory-name (format "Copy %s to " (direx:tree-name file))
                                      (direx:directory-dirname (direx:file-full-name file)))))
        (dired-copy-file (direx:file-full-name file) to nil)
        (direx:item-refresh-parent item)
        (direx:move-to-item-name-part item)))))
