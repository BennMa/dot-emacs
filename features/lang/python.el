(use-package elpy
  :disabled t
  :commands (elpy-mode)
  :init (and (fboundp 'python-mode-hook)
             (add-hook 'python-mode-hook 'elpy-mode))
  :config (elpy-enable))

(use-package yapfify
  :config
  (add-hook 'python-mode-hook 'yapf-mode))

(use-package anaconda-mode
  :diminish (anaconda-mode . " [AC]")
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  (use-package company-anaconda
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda))))

(use-package ein
  :commands (ein:notebooklist-login
             ein:notebooklist-open)
  :config
  (require 'ein)
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages))

(use-package pipenv
  :diminish (pipenv-mode . " [PE]")
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))


(require 'pyenv-mode)
(add-hook 'python-mode-hook 'pyenv-mode)
;; (use-package pyenv-mode
  ;; :disabled t
  ;; :hook (python-mode . pyenv-mode))
