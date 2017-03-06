(use-package elpy
  :commands (elpy-mode)
  :init (and (fboundp 'python-mode-hook)
             (add-hook 'python-mode-hook 'elpy-mode))
  :config (elpy-enable))
