(general-define-key (general-chord "zz") 'hydra-zoom/body)

;; ------ packages
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
  ;; :bind-keymap ("C-c w" . escreen-map)
  :commands (escreen-create-screen)
  :config
  (bind-key "e" 'escreen-goto-last-screen escreen-map)
  (bind-key "m" 'escreen-menu escreen-map)
  (escreen-install))

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

  (add-hook 'after-init-hook #'global-flycheck-mode))

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

(use-package counsel-gtags
  :bind (("M-." . counsel-gtags-find-definition)
         ;; counsel-gtags-find-reference
         ;; counsel-gtags-find-symbol
         ;; ("M-," . counsel-gtags-pop-stack)
         ))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-log-buffer-file))
  :commands (magit-status-with-prefix)
  :config
  (progn 
    (setenv "GIT_PAGER" "")
    (add-hook 'magit-mode-hook 'hl-line-mode)

    ;; (unbind-key "M-h" magit-mode-map)
    ;; (unbind-key "M-s" magit-mode-map)
    ;; (unbind-key "M-m" magit-mode-map)
    ;; (unbind-key "M-w" magit-mode-map)
    ;; (unbind-key "C-<return>" magit-file-section-map)

    ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
    ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
    (bind-key "U" #'magit-unstage-all magit-mode-map)

    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))

    (defun magit-monitor (&optional no-display)
      "Start git-monitor in the current directory."
      (interactive)
      (when (string-match "\\*magit: \\(.+?\\)\\*" (buffer-name))
        (let ((name (format "*git-monitor: %s*"
                            (match-string 1 (buffer-name)))))
          (or (get-buffer name)
              (let ((buf (get-buffer-create name)))
                (ignore-errors
                  (start-process "*git-monitor*" buf "git-monitor"
                                 "-d" (expand-file-name default-directory)))
                buf)))))
    (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

    (defun magit-status-with-prefix ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'magit-status)))

    (defun eshell/git (&rest args)
      (cond
       ((or (null args)
            (and (string= (car args) "status") (null (cdr args))))
        (magit-status-internal default-directory))
       ((and (string= (car args) "log") (null (cdr args)))
        (magit-log "HEAD"))
       (t (throw 'eshell-replace-command
                 (eshell-parse-command
                  "*git"
                  (eshell-stringify-list (eshell-flatten-list args)))))))))

;; ------ hydra
(defhydra hydra-zoom (:hint nil)
  "
^BUFFER^   ^FRAME^    ^ACTION^
_t_: +     _T_: +     _0_: reset
_s_: -     _S_: -     _q_: quit
"
  ("t" zoom-in )
  ("s" zoom-out )
  ("T" zoom-frm-in )
  ("S" zoom-frm-out )
  ("0" zoom-frm-unzoom)
  ("q" nil :color blue))
