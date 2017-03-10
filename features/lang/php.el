(general-define-key :keymaps 'php-mode-map
                    "C-," (defhydra hydra-php (:hint nil :color blue :exit t :columns 4)
                            "PHP Helper"
                            ("f" phpcbf "Code Format")
                            ("d" semantic-ia-show-doc "Show doc")
                            ("g" my/php-generate-func-doc "Generate func doc")
                            ("r" hydra-php-refactor/body "Refactor")
                            ("q" nil "Cancel"))
                    "M-." 'semantic-ia-fast-jump)

(defhydra hydra-php-refactor (:hint nil :color blue :exit t :columns 2)
  "PHP Refactor Helper"
  ("c" php-refactor--convert-local-to-instance-variable  "Covert local to instance variable")
  ("r" php-refactor--rename-local-variable  "Rename local variable")
  ("e" php-refactor--extract-method  "Extract method")
  ("o" php-refactor--optimize-use  "Optimize use")
  ("q" nil "Cancel"))

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
    (with-eval-after-load 'semantic
      (semanticdb-enable-gnu-global-databases 'php-mode))

    ;; semantic-php
    (load (expand-file-name "site-lisp/semantic-php/loaddefs" user-emacs-directory))
    ;; (add-to-list 'load-path (expand-file-name "site-lisp/metaturso-semantic-php" user-emacs-directory))
    ;; (require 'grammar-setup)

    (require 'company-semantic-ia)

    (defun my/php-mode-hook()
      (eldoc-mode t)
      (aggressive-indent-mode t)
      (semantic-mode t)
      ;; (setq-local semanticdb-ebrowse-file-match "\\.\\(php\\)")
      ;; (setq-local semanticdb-find-default-throttle '(project unloaded system recursive))
      (php-refactor-mode t)
      ;; (ede-php-autoload-mode t)
      (setq-local company-backends '(
                                     company-semantic-ia
                                      ;; :separate php-extras-company)
                                     ;; (company-dabbrev-code company-gtags company-etags company-keywords)
                                     )))

    (add-hook 'php-mode-hook 'my/php-mode-hook)
    (with-eval-after-load 'company-semantic
      (add-to-list 'company-semantic-modes 'php-mode))))

(use-package ede-php-autoload
  :commands ede-php-autoload-mode
  :config
  (progn
    (and (boundp global-ede-mode) global-ede-mode (global-ede-mode))
    (require 'ede-php-autoload-mode)))

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

(use-package php-refactor-mode
  :diminish (php-refactor-mode . "")
  :commands php-refactor-mode)

(use-package php-boris :disabled t)

(progn
  (defun my//php-write-param-doc-line (type name)
    "Insert a PHPDoc line for a param.

TYPE is the type of the parameter.
NAME is the name of the parameter."
    (insert (format " * @param %s %s\n"
                    (if (and (stringp type)
                             (not (string= type "")))
                        type
                      "mixed")
                    name)))

  (defun my//php-get-func-arguments (tag)
    "Return arguments of TAG function.

Return a result as '((type1 arg1) (type2 arg2) ... )"
    (mapcar
     (lambda (arg-tag)
       (list (semantic-tag-type arg-tag)
             (semantic-tag-name arg-tag)))
     (semantic-tag-function-arguments tag)))

  (defun my//php-align-col (rows col-number)
    "Align cells of ROWS for column COL-NUMBER.

This operation is done in place."
    (when rows
      (let ((max-length (apply 'max
                               (mapcar
                                (lambda (row) (length (nth (1- col-number) row)))
                                rows))))
        (dolist (row rows)
          (let ((cell (nth (1- col-number) row))
                (spaces ""))
            (dotimes (i (- max-length (length cell)))
              (setq spaces (concat spaces " ")))
            (setf (nth (1- col-number) row) (concat cell spaces)))))))

  (defun my//php-write-doc (doc)
    "Insert DOC at the current point."
    (let ((point nil))
      (insert "/**\n")
      ;; Insert lines
      (insert " */")
      (when point
        ;; Go to point
        )))

  (defun my/php-generate-func-doc ()
    "Generate documentation for a function tag."
    (interactive)
    (let* ((tag (semantic-current-tag))
           (args (my//php-get-func-arguments tag))
           base-point
           description-point
           end-point)
      (my//php-align-col args 1)
      (php-beginning-of-defun)
      (open-line 1)
      (setq base-point (point))
      (insert "/**\n")
      (insert " * \n")
      (dolist (arg args)
        (my//php-write-param-doc-line (nth 0 arg) (nth 1 arg)))
      (insert " *\n")
      (insert (format " * @return %s\n" (read-string "Return type : ")))
      (insert " */")
      (indent-region base-point (point)))))
