(general-define-key :keymaps 'php-mode-map
                    "C-," (defhydra hydra-php (:hint nil :color blue :exit t :columns 4)
                            "PHP Helper"
                            ("f" phpcbf "Code Format")
                            ("q" nil "Cancel")))

;; ------ packages
(use-package php-mode
  :mode (("\\.php[0-9]?\\'" . php-mode))
  :bind (:map php-mode-map
              ("<return>" . newline-and-indent)
              ("C-." . nil)
              ("C-c C-y" . yas/create-php-snippet)
              ("C-c C-f" . php-extras-eldoc-documentation-function)
              ("C-c t t" . phpunit-current-test)
              ("C-c t c" . phpunit-current-class)
              ("C-c t p" . phpunit-current-project))
  :config
  (progn
    ;; (global-ede-mode)
    ;; semantic-php
    (load (expand-file-name "site-lisp/semantic-php/loaddefs" user-emacs-directory))

    (defun my/php-mode-hook()
      (eldoc-mode t)
      (aggressive-indent-mode t)
      (semantic-mode t)
      (and (boundp global-ede-mode)
           global-ede-mode
           (ede-php-autoload-mode t))
      (setq-local company-backends '((company-ac-php-backend
                                      ;; php-extras-company
                                      company-dabbrev-code
                                      company-gtags
                                      company-keywords)
                                     company-semantic
                                     company-files
                                     company-dabbrev
                                     company-oddmuse)))
    (add-hook 'php-mode-hook 'my/php-mode-hook)
    (with-eval-after-load 'company-semantic
      (add-to-list 'company-semantic-modes 'php-mode))))

(use-package ede-php-autoload
  :commands ede-php-autoload-mode
  :config (require 'ede-php-autoload-mode))

(use-package company-php
  :commands company-ac-php-backend)

(use-package php-extras
  :commands (php-extras-company
             php-extras-insert-previous-variable
             php-extras-eldoc-documentation-function))

(use-package php-auto-yasnippets
  :commands yas/create-php-snippet)

(use-package phpunit
  :commands (phpunit-current-test
             phpunit-current-class
             phpunit-current-project))

(use-package phpcbf
  :commands phpcbf)
