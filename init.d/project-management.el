(use-package projectile
  :demand t
  :diminish projectile-mode
  :commands projectile-global-mode
  :bind-keymap ("M-p" . projectile-command-map)
  :config
  (unbind-key "C-c p" projectile-mode-map)
  (add-hook 'projectile-mode-hook
            #'(lambda () 
                (auto-highlight-symbol-mode 1)))  
  (projectile-global-mode)

  
  )

(use-package helm-projectile
  :defer 10
  :config
  (setq projectile-completion-system 'helm)
  (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on))

(use-package perspective
  :disabled t
  :config
  (persp-mode)
  (use-package persp-projectile))

;; https://github.com/sabof/project-explorer
(use-package project-explorer
  :after projectile
  :config
  (bind-key "w" 'project-explorer-toggle projectile-command-map)
  (bind-key "o" 'pe/find-file project-explorer-mode-map)
  ;; (add-hook 'project-explorer-mode-hook (lambda () (nlinum-mode -1)))
  )
