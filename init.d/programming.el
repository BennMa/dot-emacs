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
  :mode (("\\.php[0-9]?\\'" . php-mode))
  :config
  
  (use-package php-doc
    :disabled t
    :config
    (bind-key "C-c C-d" 'php-insert-doc-block php-mode-map))
  
  (unbind-key "C-." php-mode-map))

(use-package phpunit
  ;; :mode (("\\.php$" . phpunit-mode))
  :bind (:map php-mode-map
              ("C-c t t" . phpunit-current-test)
              ("C-c t c" . phpunit-current-class)
              ("C-c t p" . phpunit-current-project)))


(use-package symfony1x
  :ensure nil
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


;; ------ Html & Css

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

(use-package cc-mode
  :bind (:map c-mode-map
         ("<return>" . newline-and-indent))
  :config

  (setq-default c-electric-flag nil)

  ;; (defun my-make-CR-do-indent ()
  ;;   (define-key c-mode-base-map "<return>" 'newline-and-indent))
  ;; (add-hook 'c-initialization-hook 'my-make-CR-do-indent)
  
  )

;; ------ Javascript

;; (use-package js-comint
;;   :disabled t
;;   :config
;;   (add-hook 'js2-mode-hook '(lambda ()
;;                               (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;;                               (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;;                               (local-set-key "\C-cb" 'js-send-buffer)
;;                               (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;;                               (local-set-key "\C-cl" 'js-load-file-and-go))))

(use-package js3-mode
  :disabled t
  :mode (("\\.js\\'" . js3-mode)
         ("\\.json\\'" . js3-mode)))


(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.json\\'" . js3-mode))
  :bind (:map js2-mode-map
         ("<return>" . newline-and-indent))
  :config
  (use-package js2-refactor
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m")
    ;; (js2r-add-keybindings-with-modifier "C-s-")
    )

  (use-package skewer-mode
    :init
    (use-package simple-httpd)
    :config
    (skewer-setup)
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'web-mode-hook 'skewer-mode)))

(use-package tern
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

  (use-package company-tern
    :config
    (add-to-list 'company-backends 'company-tern)
    ;; (setq company-tern-property-marker "")
    ;; (setq company-tern-meta-as-single-line t)
    ;; (setq company-tooltip-align-annotations t)
    ))

(use-package nodejs-repl
  :disabled t
  :config
  (defun my-nodejs-repl-hook()
    (local-set-key "\C-x\C-e" 'nodejs-repl-send-last-sexp)
    (local-set-key "\C-\M-x" 'nodejs-repl-send-last-sexp-and-go)
    (local-set-key "\C-cb" 'nodejs-repl-send-buffer)
    (local-set-key "\C-c\C-b" 'nodejs-repl-send-buffer-and-go)
    (local-set-key "\C-cl" 'nodejs-repl-load-file-and-go))

  (add-hook 'js2-mode-hook 'my-nodejs-repl-hook))

;; ------ CEDET

(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (set-default 'semantic-case-fold t)
  (semantic-mode 1)
  
  ;; (semantic-add-system-include "/usr/include/boost" 'c++-mode)


  ;; You can add this to improve the parse of macro-heavy code:
  ;; (require 'semantic/bovine/c)
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
  ;;              "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")
  )

(use-package semantic-php
  :ensure nil
  :load-path "lisp/semantic-php"
  :init
  (load "semantic-php/loaddefs"))

(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))
