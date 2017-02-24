(use-package web-beautify)

(use-package js2-mode
  :mode ("\\.js\\'"   . js2-mode)
  :config
  (progn
    (unbind-key "C-c C-f" js2-mode-map)
    (unbind-key "C-c C-f" js2-mode-map)))

(use-package json-mode :ensure t :mode ("\\.json\\'" . json-mode))

(use-package js2-refactor
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    ;; (js2r-add-keybindings-with-modifier "C-s-")
    (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package js-doc
  :config (add-hook 'js2-mode-hook
                    #'(lambda ()
                        (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                        (define-key js2-mode-map "@" 'js-doc-insert-tag))))

(use-package tern :config (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
(use-package company-tern :config (add-to-list 'company-backends 'company-tern))

(use-package json-snatcher
  :config
  (progn
    (defun js-mode-bindings ()
      "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
        (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
    (add-hook 'js-mode-hook 'js-mode-bindings)
    (add-hook 'js2-mode-hook 'js-mode-bindings)
    (add-hook 'json-mode-hook 'js-mode-bindings)))
