(general-define-key "M-f" 'sp-forward-word
                    "M-b" 'sp-backward-word
                    "M-o C" 'highlight-changes-mode
                    "M-o h" 'hl-line-mode
                    "M-%"   'anzu-query-replace
                    "C-M-%" 'anzu-query-replace-regexp
                    "C-S-s" 'counsel-ag
                    "C-=" 'er/expand-region
                    "M-=" '(lambda () (interactive)
                             (save-excursion
                               (call-interactively 'er/expand-region)
                               (call-interactively 'kill-ring-save)))
                    "C-M-=" 'er/mark-defun
                    "C-M-\\" 'sp-indent-defun
                    "C-1" 'ahs-backward
                    "C-2" 'ahs-forward
                    "C->" 'mc/mark-next-like-this
                    "C-<" 'mc/mark-previous-like-this
                    "C-M->" 'mc/mark-all-like-this
                    "<f2>" 'bm-next
                    "S-<f2>" 'bm-previous
                    "C-<f2>" 'bm-toggle)

(general-define-key "C-M-f" 'sp-forward-sexp
                    "C-M-b" 'sp-backward-sexp
                    "C-M-a" 'sp-backward-up-sexp
                    "C-M-e" 'sp-up-sexp
                    ;; "C-M-a" 'sp-beginning-of-sexp
                    ;; "C-M-e" 'sp-end-of-sexp
                    ;; "C-M-k" 'sp-kill-sexp
                    ;; "C-M-t" 'sp-transpose-sexp
                    "C-M-j" '(lambda() (interactive) (delete-indentation t))
                    "C-M-t" 'move-text-down
                    "C-M-S-t" 'move-text-up
                    ;; "C-M-d" 'hungry-delete-forward
                    ;; "C-M-<backspace>" 'hungry-delete-backward
                    "<C-backspace>" 'sp-backward-kill-sexp
                    "M-d" 'blaine/contextual-kill-word
                    "<M-backspace>" 'blaine/contextual-backspace
                    "C-M-c" 'hide/show-comments-toggle)

(general-define-key "C-c i b" 'flyspell-buffer
                    "C-c i f" 'flyspell-mode
                    "C-c i c" 'ispell-comments-and-strings
                    "C-c i d" 'ispell-change-dictionary
                    "C-c i k" 'ispell-kill-ispell
                    "C-c i m" 'ispell-message
                    "C-c i r" 'ispell-region
                    ;; "C-c C-f" 'hydra-flycheck/body
                    "C-c C-e" 'mc/mark-all-like-this
                    "C-c h a" 'origami-toggle-all-nodes
                    "C-c h t" 'origami-recursively-toggle-node
                    "C-c h f" 'origami-show-only-node
                    "C-c h r" 'origami-reset
                    "C-c h o" 'narrow-to-defun
                    "C-c h O" 'widen
                    "C-c h f" 'focus-mode
                    "C-c h F" 'focus-read-only-mode
                    "C-c p" 'goto-last-change
                    "C-c n" 'goto-last-change-reverse)

;; ------ packages
(use-package swiper :commands swiper)
(use-package anzu :diminish ""
  :commands (anzu-query-replace
             anzu-query-replace-regexp)
  :config (global-anzu-mode 1))

(use-package eldoc :ensure nil :diminish ""
  :commands eldoc-mode)

(use-package expand-region
  :commands (er/expand-region
             er/mark-defun)
  :config
  (setq er/try-expand-list '( ;; er/mark-word
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

(use-package hilit-chg
  :commands highlight-changes-mode)

(use-package clean-aindent-mode
  :config
  (progn
    (electric-indent-mode -1)
    (clean-aindent-mode 1)))

(use-package highlight-parentheses :diminish ""
  :config (global-highlight-parentheses-mode))

(use-package indent-guide :diminish ""
  :disabled t
  :config
  (progn
    ;; (set-face-background 'indent-guide-face "dimgray")
   (indent-guide-global-mode)))
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package hl-line :diminish ""
  :commands hl-line-mode
  :config
  (progn
    ;; (use-package hl-line+)
    ;; (global-hl-line-mode)
    ))

(use-package auto-highlight-symbol :diminish ""
  :demand t
  :commands (ahs-backward
             ahs-forward)
  :config
  (global-auto-highlight-symbol-mode t))

(use-package ispell
  :commands (ispell-comments-and-strings
             ispell-change-dictionary
             ispell-kill-ispell
             ispell-message
             ispell-region))

(use-package flyspell :diminish " ⓢ"
  :commands (flyspell-buffer
             flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-." . nil)
              ("C-c $" . nil))
  :config
  (progn
    (flyspell-mode 1)))

(use-package flycheck :diminish " ⓒ"
  :commands (flycheck-mode
             global-flycheck-mode
             hydra-flycheck/body)
  :init (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (flycheck-add-mode 'php       'web-mode)
    (flycheck-add-mode 'php-phpmd 'web-mode)
    (flycheck-add-mode 'php-phpcs 'web-mode)

    (defalias 'flycheck-show-error-at-point-soon
      'flycheck-show-error-at-point)

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
      ("q"  nil))))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this))

(use-package origami
  :commands (origami-mode
             origami-toggle-all-nodes
             origami-recursively-toggle-node
             origami-show-only-node
             origami-reset)
  :init (add-hook 'prog-mode-hook 'origami-mode)
  :config (origami-mode 1))

(use-package focus
  :commands (focus-mode
             focus-read-only-mode))

(use-package elec-pair :disabled t
  :config (electric-pair-mode 1))
(use-package smartparens-config :ensure smartparens
  :commands (smartparens-mode
             sp-forward-symbol
             sp-backward-symbol
             sp-backward-up-sexp
             sp-up-sexp
             sp-kill-sexp
             sp-transpose-sexp
             sp-forward-word
             sp-backward-word
             sp-backward-kill-word)
  :init
  (progn
    (and (boundp 'prog-mode-hook)
         (add-hook 'prog-mode-hook 'smartparens-mode))
    (and (boundp 'markdown-mode-hook)
         (add-hook 'markdown-mode-hook 'smartparens-mode)))
  :config
  (progn
    (smartparens-mode 1)
    (defun sp-forward-word ()
      (interactive)
      (sp--forward-word))
    (defun sp-backward-word ()
      (interactive)
      (sp--backward-word))))

(use-package rainbow-mode
  :commands rainbow-mode
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package bm
  :commands (bm-next
             bm-previous
             bm-toggle)
  :init (setq bm-restore-repository-on-load t)
  :config
  (progn
    (add-hook 'after-init-hook 'bm-repository-load)
    (add-hook 'find-file-hooks 'bm-buffer-restore)
    (add-hook 'kill-buffer-hook #'bm-buffer-save)
    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))
    (add-hook 'after-save-hook #'bm-buffer-save)
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)))

;; http://ergoemacs.org/emacs/whitespace-mode.html
(use-package whitespace :diminish ""
  :commands whitespace-mode
  :init (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-line-column nil
        whitespace-style '(face trailing tab-mark lines-tail)
        ;; whitespace-display-mappings '((tab-mark 9 [9654 9] [92 9]))
        ))

(use-package goto-chg
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package hungry-delete
  :commands (hungry-delete-mode
             hungry-delete-forward
             hungry-delete-backward)
  ;; :init (add-hook 'prog-mode-hook #'hungry-delete-mode)
  )

(use-package move-text
  :commands (move-text-up
             move-text-down))

(use-package hide-comnt :ensure nil
  :commands (hide/show-comments
             hide/show-comments-toggle))

(use-package aggressive-indent
  :commands aggressive-indent-mode)

(use-package linum :ensure nil
  :commands (linum-mode)
  :init (add-hook 'prog-mode-hook 'linum-mode))
