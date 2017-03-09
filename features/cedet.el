;; refs:
;; https://tuhdo.github.io/c-ide.html
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html

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
    (defvar semanticdb-project-root-functions nil)
    (add-to-list 'semanticdb-project-root-functions 'my/projectile-project-root)

    ;; (defun my/semantic-hook ()
    ;;   (imenu-add-to-menubar "TAGS"))
    ;; (add-hook 'semantic-init-hooks 'my/semantic-hook)

    (setq semantic-default-submodes '(global-semantic-highlight-func-mode
                                      ;; global-semantic-idle-summary-mode
                                      ;; global-semantic-idle-completions-mode
                                      global-semantic-stickyfunc-mode
                                      global-semantic-idle-scheduler-mode
                                      global-semanticdb-minor-mode))))

(use-package stickyfunc-enhance
  :after semantic)

(use-package srefactor
  :commands (srefactor-refactor-at-point
             srefactor-lisp-one-line
             srefactor-lisp-format-sexp
             srefactor-lisp-format-defun
             srefactor-lisp-format-buffer))
