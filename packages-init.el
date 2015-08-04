(use-package projectile
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("M-p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))

  (use-package symfony1x
    :load-path "lisp/symfony1x"
    :init
    (setq symfony1x-mode-key-prefix "M-p ;")
    :config
    (defun symfony1x-mode-init()
      (when (member (projectile-project-name)
                    '("Master_P" "Master_Beta" "Master_G" "Master_FT"))
        (make-local-variable 'symfony1x-mode-status)
        (symfony1x-mode t)))
    (add-hook 'projectile-mode-hook 'symfony1x-mode-init))
  
  (projectile-global-mode))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
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

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))

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
                (eshell-stringify-list (eshell-flatten-list args)))))))

  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)

  :config
  (setenv "GIT_PAGER" "")

  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)

  ;; (bind-key "M-H" #'magit-show-level-2-all magit-mode-map)
  ;; (bind-key "M-S" #'magit-show-level-4-all magit-mode-map)
  (bind-key "U" #'magit-unstage-all magit-mode-map)

  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode)))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t))))


(use-package ag
  :commands (ag ag-regexp)
  :init
  (use-package helm-ag
    :commands helm-ag))


(use-package ido
  :demand t
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :bind (("C-x b" . ido-switch-buffer)
         ("C-x B" . ido-switch-buffer-other-window))
  :preface
  (eval-when-compile
    (defvar ido-require-match)
    (defvar ido-cur-item)
    (defvar ido-show-confirm-message)
    (defvar ido-selected)
    (defvar ido-final-text))

  (defun ido-smart-select-text ()
    "Select the current completed item.  Do NOT descend into directories."
    (interactive)
    (when (and (or (not ido-require-match)
                   (if (memq ido-require-match
                             '(confirm confirm-after-completion))
                       (if (or (eq ido-cur-item 'dir)
                               (eq last-command this-command))
                           t
                         (setq ido-show-confirm-message t)
                         nil))
                   (ido-existing-item-p))
               (not ido-incomplete-regexp))
      (when ido-current-directory
        (setq ido-exit 'takeprompt)
        (unless (and ido-text (= 0 (length ido-text)))
          (let ((match (ido-name (car ido-matches))))
            (throw 'ido
                   (setq ido-selected
                         (if match
                             (replace-regexp-in-string "/\\'" "" match)
                           ido-text)
                         ido-text ido-selected
                         ido-final-text ido-text)))))
      (exit-minibuffer)))

  :config
  (ido-mode 'buffer)

  (use-package ido-hacks
    :demand t
    :bind ("M-x" . my-ido-hacks-execute-extended-command)
    :config
    (ido-hacks-mode 1)

    (defvar ido-hacks-completing-read (symbol-function 'completing-read))
    (fset 'completing-read ido-hacks-orgin-completing-read-function)
    (defun my-ido-hacks-execute-extended-command (&optional arg)
      (interactive "P")
      (flet ((completing-read
              (prompt collection &optional predicate require-match
                      initial-input hist def inherit-input-method)
              (funcall ido-hacks-completing-read
                       prompt collection predicate require-match
                       initial-input hist def inherit-input-method)))
        (ido-hacks-execute-extended-command arg))))

  (use-package flx-ido
    :disabled t
    :config
    (flx-ido-mode 1))

  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" 'ido-smart-select-text
                          ido-file-completion-map))))

(use-package color-theme
  :config
  (color-theme-install
   '(nil
     ((foreground-color . "#F8F8F2")
      (background-color . "#1B1D1E")
      (cursor-color . "#F8F8F0")
      (background-mode . dark))
     (default ((t (:foreground "#F8F8F2" :background "#1B1D1E"))))
     (bold ((t (:weight bold))))
     (bold-italic ((t (:weight bold :slant italic))))
     (custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     (custom-state ((t (:foreground "#A6E22E"))))
     (italic ((t (:slant italic))))
     (region ((t (:background "#403D3D"))))
     (underline ((t (:underline t))))
     (css-selector ((t (:foreground "#F92672"))))
     (css-property ((t (:foreground "#66D9EF"))))
     (diff-added ((t (:foreground "#A6E22E" :weight bold))))
     (diff-context ((t (:foreground "#F8F8F2"))))
     (diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     (diff-indicator-added ((t (:foreground "#A6E22E"))))
     (diff-indicator-removed ((t (:foreground "#F92672"))))
     (diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
     (diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     (diff-removed ((t (:foreground "#F92672" :weight bold))))
     (escape-glyph ((t (:foreground "#E6DB74"))))
     (minibuffer-prompt ((t (:foreground "#66D9EF"))))
     (mode-line ((t (:foreground "#F8F8F2" :background "#ff0000"
                                 :box (:line-width 1 :color "#ff0000" :style released-button)))))
     (mode-line-buffer-id ((t (:foreground nil :background nil :weight semi-bold))))
     (mode-line-inactive ((t (:foreground "#BCBCBC" :background "#1E1C1B"
                                          :box (:line-width 1 :color "#232526")))))
     (mode-line-mousable ((t (:foreground "#BCBCBC" :background "#ff0000"))))
     (mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))
     (font-lock-builtin-face ((t (:foreground "#A6E22E"))))
     (font-lock-comment-face ((t (:foreground "#465457" :slant italic))))
     (font-lock-comment-delimiter-face ((t (:foreground "#465457" :slant italic))))
     (font-lock-constant-face ((t (:foreground "#AE81FF"))))
     (font-lock-doc-face ((t (:foreground "#E6DB74" :slant italic))))
     (font-lock-function-name-face ((t (:foreground "#57d001" :height 160)))) ;; #F92672
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-negation-char-face ((t (:weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "#A6E22E"))))
     (font-lock-regexp-grouping-backslash ((t (:weight bold))))
     (font-lock-regexp-grouping-construct ((t (:weight bold))))
     (font-lock-string-face ((t (:foreground "#E6DB74"))))
     (font-lock-type-face ((t (:foreground "#66D9EF")))) ;; #66D9EF
     (font-lock-variable-name-face ((t (:foreground "#F92672"))))
     (font-lock-warning-face ((t (:foreground "#FFFFFF"
                                              :background "#333333"))))
     (fringe ((t (:background "#232526"))))
     (highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     (hl-line ((t (:background "#293739"))))
     (icompletep-choices ((t (:foreground "#F92672"))))
     (icompletep-determined ((t (:foreground "#A6E22E"))))
     (icompletep-keys ((t (:foreground "#F92672"))))
     (icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     (isearch ((t (:foreground "#C4BE89" :background "#000000"))))
     (isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     (lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
     (markdown-italic-face ((t (:slant italic))))
     (markdown-bold-face ((t (:weight bold))))
     (markdown-header-face ((t (:weight normal))))
     (markdown-header-face-1 ((t (:foreground "#66D9EF"))))
     (markdown-header-face-2 ((t (:foreground "#F92672"))))
     (markdown-header-face-3 ((t (:foreground "#A6E22E"))))
     (markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     (markdown-header-face-5 ((t (:foreground "#E6DB74"))))
     (markdown-header-face-6 ((t (:foreground "#66D9EF"))))
     (markdown-inline-code-face ((t (:foreground "#66D9EF"))))
     (markdown-list-face ((t (:foreground "#A6E22E"))))
     (markdown-blockquote-face ((t (:slant italic))))
     (markdown-pre-face ((t (:foreground "#AE81FF"))))
     (markdown-link-face ((t (:foreground "#66D9EF"))))
     (markdown-reference-face ((t (:foreground "#66D9EF"))))
     (markdown-url-face ((t (:foreground "#E6DB74"))))
     (markdown-link-title-face ((t (:foreground "#F92672"))))
     (markdown-comment-face ((t (:foreground "#465457"))))
     (markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
     (mumamo-background-chunk-major ((t (:background "#272822"))))
     (mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
     (outline-1 ((t (:foreground "#66D9EF"))))
     (outline-2 ((t (:foreground "#F92672"))))
     (outline-3 ((t (:foreground "#A6E22E"))))
     (outline-4 ((t (:foreground "#AE81FF"))))
     (outline-5 ((t (:foreground "#E6DB74"))))
     (outline-6 ((t (:foreground "#66D9EF"))))
     (outline-7 ((t (:foreground "#F92672"))))
     (outline-8 ((t (:foreground "#A6E22E"))))
     (secondary-selection ((t (:background "#272822"))))
     (show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
     (show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
     (widget-inactive-face ((t (:background "#ff0000"))))
     (woman-addition ((t (:foreground "#AE81FF"))))
     (woman-bold ((t (:foreground "#F92672"))))
     (woman-italic ((t (:foreground "#A6E22E"))))
     (woman-unknown ((t (:foreground "#66D9EF"))))
     )))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package window-number
  :config
  (unbind-key "C-x C-j" window-number-mode-map)
  (window-number-mode)
  (window-number-meta-mode))

(use-package auto-highlight-symbol
  :bind (("C-1" . ahs-backward)
         ("C-2" . ahs-forward))
  :config
  (global-auto-highlight-symbol-mode t))

(use-package web-mode
  :mode (("\\.php[0-9]?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.tpl\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (auto-highlight-symbol-mode 1)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  ;;(add-hook 'web-mode-hook '(lambda() (my-web-mode-hook))
  )
