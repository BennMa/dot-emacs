;; ------ Common
(use-package flycheck
  :disabled t
  :demand t
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (flycheck-add-mode 'php 'web-mode)
  (flycheck-add-mode 'php-phpmd 'web-mode)
  (flycheck-add-mode 'php-phpcs 'web-mode)

  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point)
  
  (global-flycheck-mode))

;; ------ PHP
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)))

(use-package php-mode
  :ensure t
  :mode (("\\.php[0-9]?\\'" . php-mode))
  :config
  
  (use-package php-doc
    :config
    (bind-key "C-c C-d" 'php-insert-doc-block php-mode-map))
  
  (unbind-key "C-." php-mode-map))


(use-package symfony1x
  :load-path "lisp/symfony1x"
  :commands symfony1x-mode
  :init
  (setq symfony1x-mode-key-prefix "C-; ;")
  (add-hook 'projectile-mode-hook
            #'(lambda ()
                (when (and (buffer-file-name)
                           (string-match "\\/Master_\\(?:Service\\|Beta\\|Community\\|FT\\)\\/" (buffer-file-name)))
                  (make-local-variable 'symfony1x-mode-status)
                  (symfony1x-mode t)))))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config
  )

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  ;; (js2r-add-keybindings-with-modifier "C-s-")
  )

(use-package simple-httpd :ensure t)
(use-package skewer-mode :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package css-mode
  :mode "\\.css\\'")

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))


;; ------ C/C++
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))


;; ------ Erlang
(use-package erlang
  :mode (("^\\.erlang$" . erlang-mode)
         ("\\.app$" . erlang-mode)
         ("\\.app.src$" . erlang-mode)
         ("\\.erl$" . erlang-mode)
         ("\\.es$" . erlang-mode)
         ("\\.escript$" . erlang-mode)
         ("\\.eterm$" . erlang-mode)
         ("\\.script$" . erlang-mode)
         ("\\.yaws$" . erlang-mode))
  :config
  (use-package edts-mode
    :load-path "site-lisp/edts/elisp/edts"
    :config
    ;; remove eproject hooks which will effect other develop environment, here
    ;; also can use-package eproject first, and set up the file types to nil
    (remove-hook 'find-file-hook #'eproject-maybe-turn-on)
    (remove-hook 'dired-mode-hook #'eproject-maybe-turn-on)
    (remove-hook 'after-change-major-mode-hook #'eproject--after-change-major-mode-hook)
    (remove-hook 'after-save-hook #'eproject--after-save-hook)

    (defun edts-erlang-mode-hook ()
      (when (buffer-file-name)
        (eproject-maybe-turn-on)
        (edts-mode t)))

    (add-hook 'erlang-mode-hook 'edts-erlang-mode-hook)))

;; ------ Python
;; (use-package-force python-mode
;;   :demand t
;;   :commands python-mode
;;   :mode ("^\\.py$" . python-mode)
;;   :interpreter ("python[0-9.]*" . python-mode))

(use-package elpy
  :config
  (elpy-enable))


;; ------ C/C++
