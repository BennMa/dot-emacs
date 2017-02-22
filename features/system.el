(cond
 ((eq system-type 'darwin) ;; mac osx
  (progn
    (defvar osx-use-option-as-meta t)

    (when (display-graphic-p)
      (setq mac-command-key-is-meta nil)
      (setq mac-command-modifier 'super)
      (when osx-use-option-as-meta
        ;; Treat option as meta
        (setq mac-option-key-is-meta t))
      (setq mac-option-modifier (if osx-use-option-as-meta 'meta nil))
      ;; Keybindings
      (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-w") 'delete-window)
      (global-set-key (kbd "s-W") 'delete-frame)
      (global-set-key (kbd "s-n") 'make-frame)
      (global-set-key (kbd "s-z") 'undo-tree-undo)
      (global-set-key (kbd "s-s")
                      (lambda ()
                        (interactive)
                        (call-interactively (key-binding "\C-x\C-s"))))
      (global-set-key (kbd "s-Z") 'undo-tree-redo)
      (with-eval-after-load 'term
        (define-key term-raw-map (kbd "s-v") 'term-paste)))

    (use-package osx-trash :ensure t
      :config (setq delete-by-moving-to-trash t))
    (use-package pbcopy :ensure t)
    (use-package reveal-in-osx-finder :ensure t
      :bind (("s-o" . reveal-in-osx-finder)))))

 ((eq system-type 'gnu/linux) ;; linux
  (progn
    (use-package cua
      :disabled t
      :config (cua-mode 1)))))
