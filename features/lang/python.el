;; useful links:
;; https://steelkiwi.com/blog/emacs-configuration-working-python/

;; this package requires python libs:
;; - jedi
;; - pipenv
;; - yapf
;; - virtualenv

(use-package elpy
  :disabled
  :commands (elpy-mode)
  :hook (python-mode . elpy-mode)
  :config (elpy-enable))

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package anaconda-mode
  :diminish (anaconda-mode . " [AC]")
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :bind (:map anaconda-mode-map
              ("M-?" . anaconda-mode-show-doc)
              ("M-." . anaconda-mode-find-definitions)
              ("M-," . anaconda-mode-go-back)
              ("C-c a" . anaconda-mode-find-assignments)
              ("C-c r" . anaconda-mode-find-references)
              ("C-M-i" . anaconda-mode-complete)))

(use-package ein
  :commands (ein:run
             ein:login)
  :config
  (load "ein-autoloads"))

(use-package pipenv
  :diminish (pipenv-mode . " [PE]")
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-default)

  ;; fix bug about void function of flycheck-default-executable-find
  (if (not (functionp 'flycheck-default-executable-find))
      (defun flycheck-default-executable-find (command)
        (executable-find command))))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode)
  :config
  (unbind-key "C-c C-s" pyenv-mode-map)
  (unbind-key "C-c C-u" pyenv-mode-map))

(use-package company-anaconda)
;; (use-package company-jedi)
(defun my/python-mode-hook ()
  (eval-after-load "company"
    (progn
      (make-local-variable 'company-backends)
      '(add-to-list 'company-backends 'company-anaconda))))
(add-hook 'python-mode-hook 'my/python-mode-hook)
