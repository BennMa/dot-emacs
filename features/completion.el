(use-package company
  :diminish (company-mode . "")
  :config
  (progn
    ;; From https://github.com/company-mode/company-mode/issues/87
    ;; See also https://github.com/company-mode/company-mode/issues/123
    (defadvice company-pseudo-tooltip-unless-just-one-frontend
        (around only-show-tooltip-when-invoked activate)
      (when (company-explicit-action-p)
        ad-do-it))
    (global-company-mode)
    (use-package company-c-headers
      :config (add-to-list 'company-backends 'company-c-headers))

    (unbind-key "<tab>" company-active-map)))

(use-package yasnippet
  :demand t
  :diminish (yas-minor-mode . "")
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y s"   . yas-insert-snippet)
         ("C-c y v"   . yas-visit-snippet-file))
  :config
  (progn
    (yas-load-directory "~/.emacs.d/snippets/")
    ;; (unbind-key "C-i" yas-minor-mode-map)
    ;; (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)
    (yas-global-mode)))

(use-package auto-yasnippet
  :bind (("C-c y c" . aya-create)
         ("C-c y y" . aya-expand)))

(use-package yatemplate :ensure t
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (progn
    (auto-insert-mode)
    (setq auto-insert-alist nil)
    (yatemplate-fill-alist)))
