(defvar my--completion-modes '(prog-mode-hook
                               markdown-mode-hook
                               ;; org-mode-hook
                               ))

(dolist (mode-map '(prog-mode-map
                    markdown-mode-map
                    ;; org-mode-map
                    ))
  (general-define-key :keymaps mode-map
                      "<tab>" 'my/tab-indent-or-complete))

(general-define-key "M-/" 'hippie-expand
                    ;; "C-<return>" . 'company-complete-common
                    "C-c y n" 'aya-create
                    "C-c y p" 'aya-persist-snippet)

(general-define-key :keymaps 'company-active-map
                    "M-h" 'company-quickhelp-manual-begin
                    "<tab>" 'my/expand-snippet-or-complete-selection)

(general-define-key :keymaps 'yas-keymap
                    "<tab>" 'my/tab-complete-or-next-field
                    ;; "C-<tab>" 'yas-next-field
                    "C-g" 'my/abort-company-or-yas)

(general-define-key :keymaps 'yas-minor-mode-map "<tab>" nil)

(progn ;; helper functions
  (defun my/tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      (indent-for-tab-command)
      (if (or (not yas/minor-mode)
              (null (my//do-yas-expand)))
          (if (my//check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (indent-for-tab-command)))))))))

  (defun my/abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (defun my/expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (my//do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun my/tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (my//do-yas-expand)))
        (if company-candidates
            (company-complete-selection)
          (if (my//check-expansion)
              (progn
                (company-manual-begin)
                (if (null company-candidates)
                    (progn
                      (company-abort)
                      (yas-next-field))))
            (yas-next-field)))))

  (defun my//do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun my//check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil))))))

;; ------ packages
;; https://github.com/company-mode/company-mode/issues/350
(use-package company :diminish ""
  :commands (company-mode)
  :init (multiple-mode-add-hook my--completion-modes 'company-mode)
  :config
  (progn
    ;; From https://github.com/company-mode/company-mode/issues/87
    ;; See also https://github.com/company-mode/company-mode/issues/123
    (defadvice company-pseudo-tooltip-unless-just-one-frontend
        (around only-show-tooltip-when-invoked activate)
      (when (company-explicit-action-p)
        ad-do-it))))

(use-package company-quickhelp
  :config
  (progn
    (setq company-frontends
          (delq 'company-echo-metadata-frontend company-frontends))

    (add-hook 'company-mode-hook 'company-quickhelp-mode)))

(use-package company-statistics
  :config (add-hook 'company-mode-hook 'company-statistics-mode))

;; http://joaotavora.github.io/yasnippet/snippet-development.html
(use-package yasnippet :diminish (yas-minor-mode . " Ⓨ")
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :commands yas-minor-mode
  :init (multiple-mode-add-hook my--completion-modes 'yas-minor-mode)
  :config
  (progn
    (setq yas-key-syntaxes (remove "w" yas-key-syntaxes))
    (yas-load-directory "~/.emacs.d/snippets/")

    ;; (defvar-local yas--expandable-keys-overlay nil)
    ;;     (defun yas-show-expand-keys ()
    ;;       "Put overlay on text which is an expandable snippet key.
    ;; This function is intended to be added to `post-command-hook'."
    ;;       (let ((keys-at-point (and yas-minor-mode (yas--templates-for-key-at-point)))
    ;;             (have-overlay (overlayp (buffer-local-value 'yas--expandable-keys-overlay (current-buffer)))))
    ;;         (if keys-at-point
    ;;             (let ((beg (nth 1 keys-at-point))
    ;;                   (end (nth 2 keys-at-point)))
    ;;               (if have-overlay
    ;;                   (move-overlay yas--expandable-keys-overlay beg end)
    ;;                 (setq-local yas--expandable-keys-overlay
    ;;                             (make-overlay beg end)))
    ;;               (overlay-put yas--expandable-keys-overlay 'face '(:box t)))
    ;;           (when have-overlay
    ;;             (delete-overlay yas--expandable-keys-overlay)))))
    ;;     (add-hook 'post-command-hook #'yas-show-expand-keys)
    ))

(use-package auto-yasnippet
  :after yasnippet
  :commands (aya-create
             aya-expand
             aya-persist-snippet))

(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (progn
    (auto-insert-mode)
    (setq auto-insert-alist nil)
    (yatemplate-fill-alist)))

(use-package abbrev :ensure nil
  :diminish ""
  :init
  (progn
    (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
    (setq-default abbrev-mode t)))

(use-package hippie-exp
  :commands hippie-expand
  :config
  (progn
    (with-eval-after-load 'yasnippet
      (push 'yas-hippie-try-expand hippie-expand-try-functions-list))))
