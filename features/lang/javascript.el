(general-define-key :keymaps 'js2-mode-map
                    "@" 'js-doc-insert-tag
                    "M-." 'tern-find-definition
                    "M-," 'tern-pop-find-definition
                    "C-," (defhydra hydra-javascript (:hint nil :color blue :exit t :columns 4)
                            "Javascript Helper"
                            ("f" web-beautify-js "Code Format")
                            ("i" js-doc-insert-function-doc "Insert Doc")
                            ("t" hydra-tern/body "Tern")
                            ("n" hydra-nodejs-repl/body "Nodejs Repl")
                            ("r" nil "Refactor")
                            ("q" nil "Cancel")))

(defhydra hydra-tern (:hint nil :color blue :exit t :columns 3)
  "Tern Helper"
  ("." tern-find-definition "Find definition")
  ("," tern-pop-find-definition "Pop find")
  (">" tern-find-definition-by-name "Find definition by name")
  ("s" tern-use-server "Use server")
  ("h" tern-highlight-refs "Highlight refs")
  ("d" tern-get-docs "Get docs")
  ("t" tern-get-type "Get type")
  ("r" tern-rename-variable "Rename Var")
  ("q" nil "Cancel"))

(defhydra hydra-nodejs-repl (:hint nil :color blue :exit t :columns 3)
  "Nodejs Repl Helper"
  ("e" nodejs-repl-send-last-sexp "Send last sexp")
  ("r" nodejs-repl-send-region "Send region")
  ("l" nodejs-repl-load-file "Load file")
  ("s" nodejs-repl-switch-to-repl "Swith to repl")
  ("q" nil "Cancel"))

(general-define-key :keymaps 'json-mode-map
                    "C-," 'hydra-json/body
                    "C-c C-g" 'jsons-print-path)

(defhydra hydra-json (:hint nil :color blue :exit t :columns 4)
  "Json Helper"
  ("f" web-beautify-js  "Code Format")
  ("q" nil "Cancel"))

;; ------ packages
(use-package js2-mode
  :mode ("\\.js\\'"   . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (add-hook 'js2-mode-hook #'aggressive-indent-mode)
    (add-hook 'js2-mode-hook #'skewer-mode)
    (add-hook 'js2-mode-hook #'(lambda () (tern-mode t)))
    ))

(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

(use-package js2-refactor
  :commands js2-refactor-mode
  :config
  (progn
    ;; (js2r-add-keybindings-with-modifier "C-s-")
    (js2r-add-keybindings-with-prefix "C-c C-m")))

(use-package js-doc
  :commands (js-doc-insert-function-doc
             js-doc-insert-tag))

;; http://ternjs.net/doc/manual.html#server_api
;; https://atom.io/packages/atom-ternjs
;; {
;;   "ecmaVersion": 6,
;;   "libs": [
;;     "browser"
;;   ],
;;   "loadEagerly": [
;;     "path/to/your/js/**/*.js"
;;   ],
;;   "dontLoad": [
;;     "node_modules/**",
;;     "path/to/your/js/**/*.js"
;;   ],
;;   "plugins": {
;;     "modules": {},
;;     "es_modules": {},
;;     "node": {},
;;     "doc_comment": {
;;       "fullDocs": true,
;;       "strong": true
;;     }
;;   }
;; }
(use-package tern
  :commands (tern-mode)
  :config
  (progn
    ;; https://github.com/syl20bnr/spacemacs/pull/3465/commits/73031fd7f20c2b5c5fb55a067f0c1fddc387152c
    (add-to-list 'tern-command "--no-port-file" 'append)

    (use-package company-tern
      :config (add-to-list 'company-backends 'company-tern))

    (defun delete-tern-process ()
      (interactive)
      (delete-process "Tern"))))

(use-package json-snatcher
  :commands jsons-print-path)

(use-package web-beautify
  :commands (web-beautify-js
             web-beautify-css
             web-beautify-html))

(use-package nodejs-repl
  :commands (nodejs-repl-send-last-sexp
             nodejs-repl-send-region
             nodejs-repl-load-file
             nodejs-repl-switch-to-repl))

(use-package skewer-mode
  :commands (skewer-mode
             skewer-css-mode
             skewer-html-mode))
