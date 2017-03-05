;; refs: https://tuhdo.github.io/c-ide.html

(use-package semantic :ensure nil
  :commands (semantic-mode)
  :config
  (progn
    (setq semanticdb-project-root-functions '(projectile-project-p))
    (setq semantic-default-submodes '(global-semantic-idle-summary-mode
                                      global-semantic-stickyfunc-mode
                                      global-semantic-idle-scheduler-mode
                                      global-semanticdb-minor-mode))))

(use-package srefactor :disabled t)
(use-package stickyfunc-enhance :disabled t)
