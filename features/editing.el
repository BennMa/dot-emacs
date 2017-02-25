(general-define-key "C-S-s"  'counsel-ag)

;; ------ packages
(use-package swiper :commands swiper)
(use-package anzu
    :diminish (anzu-mode . "")
    :bind (("M-%" . anzu-query-replace)
           ("C-M-%" . anzu-query-replace-regexp))
    :config (global-anzu-mode 1))

(use-package eldoc
  :ensure nil
  :diminish ""
  :commands eldoc-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (setq er/try-expand-list '(;; er/mark-word
                             er/mark-symbol
                             er/mark-symbol-with-prefix
                             er/mark-next-accessor
                             er/mark-method-call
                             er/mark-inside-quotes
                             ;; er/mark-outside-quotes
                             ;; er/mark-inside-pairs
                             er/mark-outside-pairs
                             er/mark-comment
                             er/mark-url
                             er/mark-email
                             er/mark-defun)))

(use-package hilit-chg :bind ("M-o C" . highlight-changes-mode))

(use-package clean-aindent-mode
  :config
  (progn
    (electric-indent-mode -1)
    (clean-aindent-mode 1)))

(use-package highlight-parentheses
  :diminish (highlight-parentheses-mode . "")
  :config
  (global-highlight-parentheses-mode))
(use-package indent-guide
  :diminish (indent-guide-mode . "")
  :config
  ;; (set-face-background 'indent-guide-face "dimgray")
  (indent-guide-global-mode))
(use-package hl-line
  :diminish (hl-line-mode . "")
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  ;; (use-package hl-line+)
  ;; (global-hl-line-mode)
  )

(use-package auto-highlight-symbol
  :demand t
  :diminish (auto-highlight-symbol-mode . "")
  :bind (("C-1" . ahs-backward)
         ("C-2" . ahs-forward))
  :config
  (global-auto-highlight-symbol-mode t)
  (unbind-key "M-<left>" auto-highlight-symbol-mode-map)
  (unbind-key "M-<right>" auto-highlight-symbol-mode-map)
  (unbind-key "M-S-<left>" auto-highlight-symbol-mode-map)
  (unbind-key "M-S-<right>" auto-highlight-symbol-mode-map))

(use-package dedicated :bind ("C-c D" . dedicated-mode))
(use-package sticky-windows :ensure nil
  :commands (sticky-window-delete-window
             sticky-window-delete-other-windows)
  :bind (;;("C-. S" . sticky-window-keep-window-visible)
         ("C-x 0" . sticky-window-delete-window)
         ("C-x 1" . sticky-window-delete-other-windows)))

(use-package flyspell
  :diminish (flyspell-mode . "ⓢ")
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

(use-package flycheck
  :diminish (flycheck-mode . " ⓒ")
  ;; :commands (flycheck-mode global-flycheck-mode)
  :config
  (flycheck-add-mode 'php       'web-mode)
  (flycheck-add-mode 'php-phpmd 'web-mode)
  (flycheck-add-mode 'php-phpcs 'web-mode)

  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point)

  (add-hook 'after-init-hook #'global-flycheck-mode)

  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-e" . mc/mark-all-like-this)))

(use-package hideshow
  :bind (("C-c h a" . my-toggle-hideshow-all)
         ("C-c h t" . hs-toggle-hiding)
         ("C-c h o" . narrow-to-defun)
         ("C-c h O" . widen))
  :commands hs-minor-mode
  :init (add-hook 'prog-mode-hook 'hs-minor-mode)
  :config
  (progn
    (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
    (defun my-toggle-hideshow-all ()
      "Toggle hideshow all."
      (interactive)
      (setq my-hs-hide (not my-hs-hide))
      (if my-hs-hide
          (hs-hide-all)
        (hs-show-all)))))

(use-package focus
  :commands (focus-mode focus-read-only-mode)
  :bind (("C-c h f" . focus-mode)
         ("C-c h F" . focus-read-only-mode)))

(use-package elec-pair :disabled t :config (electric-pair-mode 1))
(use-package smartparens-config :ensure smartparens
  :commands (smartparens-mode)
  :bind (("C-M-f" . sp-forward-symbol)
         ("C-M-b" . sp-backward-symbol)
         ("C-M-a" . sp-backward-up-sexp)
         ("C-M-e" . sp-up-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-t" . sp-transpose-sexp))
  :bind* (("M-f" . sp-forward-word)
          ("M-b" . sp-backward-word)
          ("<C-backspace>" . sp-backward-kill-word))
  :init
  (progn
    (and (boundp 'prog-mode-hook)
         (add-hook 'prog-mode-hook 'smartparens-mode))
    (and (boundp 'markdown-mode-hook)
         (add-hook 'markdown-mode-hook 'smartparens-mode))
    (defun sp-forward-word ()
      (interactive)
      (sp--forward-word))
    (defun sp-backward-word ()
      (interactive)
      (sp--backward-word))))

(use-package rainbow-mode
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package bm
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle))
  :init (setq bm-restore-repository-on-load t)
  :config
  (progn
    (add-hook' after-init-hook 'bm-repository-load)
    (add-hook 'find-file-hooks 'bm-buffer-restore)
    (add-hook 'kill-buffer-hook #'bm-buffer-save)
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))
    (add-hook 'after-save-hook #'bm-buffer-save)
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)))

(use-package whitespace
  :diminish ""
  :commands whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  ;http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-line-column nil
        whitespace-style '(face trailing tab-mark lines-tail)
        ;; whitespace-display-mappings '((tab-mark 9 [9654 9] [92 9]))
        ))

(use-package goto-chg :ensure t
  :commands goto-last-change
  ;; complementary to
  ;; C-x r m / C-x r l
  ;; and C-<space> C-<space> / C-u C-<space>
  :bind* (("C-c p" . goto-last-change)
          ("C-c n" . goto-last-change-reverse)))
