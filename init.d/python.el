(req-package-force python-mode
  :demand t
  :commands python-mode
  :mode ("^\\.py$" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode))

(req-package-force elpy
  :config
  (elpy-enable))
