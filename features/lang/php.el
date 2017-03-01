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
  (add-hook 'php-mode-hook 'eldoc-mode))

(use-package php-extras
  :after php-mode
  :commands (php-extras-insert-previous-variable
             php-extras-eldoc-documentation-function))

(use-package php-auto-yasnippets
  :commands yas/create-php-snippet
  :bind (:map php-mode-map
              ("C-c C-y" . yas/create-php-snippet)))

(use-package phpunit
  :commands (phpunit-current-test
             phpunit-current-class
             phpunit-current-project))

(use-package phpcbf
  :commands phpcbf)
