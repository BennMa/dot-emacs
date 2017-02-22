(use-package php-mode
  :ensure t
  :mode (("\\.php[0-9]?\\'" . php-mode))
  :bind (:map php-mode-map
              ("<return>" . newline-and-indent))
  :config
  (add-hook 'php-mode-hook 'eldoc-mode)
  (unbind-key "C-." php-mode-map))

(use-package php-extras :defer t)
(use-package php-auto-yasnippets
  :bind (:map php-mode-map
              ("C-c C-y" . yas/create-php-snippet)))
(use-package phpunit
  :bind (:map php-mode-map
              ("C-c t t" . phpunit-current-test)
              ("C-c t c" . phpunit-current-class)
              ("C-c t p" . phpunit-current-project)))
(use-package phpcbf :defer t)
