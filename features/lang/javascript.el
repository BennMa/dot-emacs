(general-define-key :keymaps 'js2-mode-map
                    "C-," 'hydra-javascript/body
                    "@" 'js-doc-insert-tag
                    "M-." 'tern-find-definition
                    "M-," 'tern-pop-find-definition
                    "C-c C-f" nil
                    "C-c i" 'js-doc-insert-function-doc)

(general-define-key :keymaps 'json-mode-map
                    "C-," 'hydra-json/body
                    "C-c C-g" 'jsons-print-path)


(defhydra hydra-javascript (:hint nil :color blue :exit t :columns 4)
  "Javascript Helper"
  ("f" web-beautify-js  "Code Format")
  ("t" hydra-tern/body "Tern")
  ("q" nil "Cancel"))

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
    (add-hook 'js2-mode-hook #'(lambda () (tern-mode t)))))

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

(use-package tern
  :commands tern-mode
  :config
  (progn
    (use-package company-tern
      :config (add-to-list 'company-backends 'company-tern))))

(use-package json-snatcher
  :commands jsons-print-path)

(use-package web-beautify
  :commands (web-beautify-js
             web-beautify-css
             web-beautify-html))
