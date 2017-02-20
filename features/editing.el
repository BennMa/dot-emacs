(use-package clean-aindent-mode :config (clean-aindent-mode))
(use-package expand-region :bind ("C-=" . er/expand-region))
(use-package hilit-chg :bind ("M-o C" . highlight-changes-mode))

(use-package ace-jump-mode
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)))

(use-package dedicated :bind ("C-. D" . dedicated-mode))
(use-package sticky-windows
  :bind (;;("C-. S" . sticky-window-keep-window-visible)
         ("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)))

(use-package escreen
  :bind-keymap ("C-c w" . escreen-map)
  :commands (escreen-create-screen)
  :config
  (bind-key "e" 'escreen-goto-last-screen escreen-map)
  (bind-key "m" 'escreen-menu escreen-map)
  (escreen-install))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode))
  :init
  (use-package ispell
    :bind (("C-c i c" . ispell-comments-and-strings)
           ("C-c i d" . ispell-change-dictionary)
           ("C-c i k" . ispell-kill-ispell)
           ("C-c i m" . ispell-message)
           ("C-c i r" . ispell-region)))
  :config
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-c $" flyspell-mode-map))

(use-package auto-highlight-symbol
  :demand t
  :bind (("C-1" . ahs-backward)
         ("C-2" . ahs-forward))
  :config
  (global-auto-highlight-symbol-mode t)
  (unbind-key "M-<left>" auto-highlight-symbol-mode-map)
  (unbind-key "M-<right>" auto-highlight-symbol-mode-map)
  (unbind-key "M-S-<left>" auto-highlight-symbol-mode-map)
  (unbind-key "M-S-<right>" auto-highlight-symbol-mode-map))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-e" . mc/mark-all-like-this)))

(use-package hideshow
  :bind (("C-c C-h" . my-toggle-hideshow-all)
         ("C-c C-t" . hs-toggle-hiding)
         ("C-x n d" . narrow-to-defun)
         ("C-x n w" . widen))
  :config
  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun my-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq my-hs-hide (not my-hs-hide))
    (if my-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (add-hook 'js2-mode-hook 'hs-minor-mode)
  (add-hook 'web-mode-hook 'hs-minor-mode)
  (add-hook 'php-mode-hook 'hs-minor-mode)
  (add-hook 'c-mode-hook 'hs-minor-mode)
  
  ;; (add-hook 'projectile-mode-hook 'hs-minor-mode)
  )
