;; refs: https://tuhdo.github.io/c-ide.html

(use-package ede :ensure nil
  :commands (global-ede-mode))

(use-package semantic :ensure nil
  :commands (semantic-mode)
  :config
  (progn
    (defun my/projectile-project-root (dir)
      (let ((default-directory dir))
        (if (fboundp 'projectile-project-root)
            (projectile-project-root)
          dir)))
    (setq semantic-default-submodes '(global-semantic-idle-summary-mode
                                      global-semantic-stickyfunc-mode
                                      global-semantic-idle-scheduler-mode
                                      global-semanticdb-minor-mode))
    (and (boundp 'semanticdb-project-root-functions)
         (add-to-list 'semanticdb-project-root-functions 'my/projectile-project-root))))

(use-package stickyfunc-enhance
  :after semantic)

(use-package srefactor :disabled t)
