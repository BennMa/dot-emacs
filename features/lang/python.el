(use-package elpy
  :commands (elpy-mode)
  :init (and (fboundp 'python-mode-hook)
             (add-hook 'python-mode-hook 'elpy-mode))
  :config (elpy-enable))

;; (use-package ein
;;   :commands (ein:notebooklist-open)
;;   :config
;;   (require 'ein)
;;   (require 'ein-loaddefs)
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages))
