(use-package helm-config
  :defer 5
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ("C-x f"   . helm-multi-files)
         ("M-s b"   . helm-occur)
         ("M-s n"   . my-helm-find)
         ("M-H"     . helm-resume))

  :preface
  (defun my-helm-find ()
    (interactive)
    (helm-find nil))

  :config
  (use-package helm-commands)
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-ls-git)

  ;; (use-package helm-match-plugin
  ;;   :config
  ;;   (helm-match-plugin-mode 1))

  (helm-autoresize-mode 1)

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  (bind-key "M-v" 'helm-previous-page helm-map)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package projectile
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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))


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
     (font-lock-function-name-face ((t (:foreground "#57d001")))) ;; #F92672  :height 160
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
  :demand t
  :bind (("C-1" . ahs-backward)
         ("C-2" . ahs-forward))
  :config
  (global-auto-highlight-symbol-mode t))

(use-package cus-edit
  :defer 5
  :config
  (use-package initsplit))

(use-package org-init
  :commands my-org-startup
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-m"   . org-smart-capture)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read))
  :defer 30
  :config
  ;; (run-with-idle-timer 300 t 'jump-to-org-agenda)
  (my-org-startup)
  (add-hook 'org-mode-hook #'(lambda () (yas-minor-mode 1))))


(use-package dired
  :bind ("C-c J" . dired-double-jump)
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  :config
  (use-package dired-x)
  (use-package dired+
    :config
    (unbind-key "M-s f" dired-mode-map))

  (bind-key "l" 'dired-up-directory dired-mode-map)

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  (bind-key "<tab>" 'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" 'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package dired-toggle
  :load-path "site-lisp/dired-toggle"
  :bind ("C-. d" . dired-toggle)
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (visual-line-mode 1)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :config
  (add-hook 'dired-toggle-mode-hook #'my-dired-toggle-mode-hook))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("<backtab>" . yas-expand)
         ("C-c y TAB" . yas-expand)
         ("C-c y s"   . yas-insert-snippet)
         ("C-c y n"   . yas-new-snippet)
         ("C-c y v"   . yas-visit-snippet-file))
  :preface
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d)
                       (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  :config
  (yas-load-directory "~/.emacs.d/snippets/")

  (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap))

(use-package bookmark
  :defer 10
  :config
  (use-package bookmark+))

(use-package web-mode
  :mode (("\\.php[0-9]?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.tpl\\'" . web-mode))
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (auto-highlight-symbol-mode 1))
  (add-hook 'web-mode-hook '(lambda() (my-web-mode-hook))))

(use-package css-mode
  :mode "\\.css\\'")

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package fold-this
  :disabled t
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package dedicated
  :bind ("C-. D" . dedicated-mode))

(use-package escreen
  :bind-keymap ("C-c w" . escreen-map)
  :commands (escreen-create-screen)
  :config
  (bind-key "e" 'escreen-goto-last-screen escreen-map)
  (bind-key "m" 'escreen-menu escreen-map)
  (escreen-install))

(use-package hl-line
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  (use-package hl-line+))

(use-package ggtags
  :disabled t
  :commands ggtags-mode
  :diminish ggtags-mode)

(use-package etags
  :commands (etags-select-find-tag-at-point)
  :bind ("M-?" . etags-select-find-tag)
  :config
  (bind-key* "M-." 'etags-select-find-tag-at-point)
  (bind-key* "M-p R" 'my-regenerate-tags)

  (defun my-regenerate-tags (dir)
    "Create tags file."
    (interactive (list (replace-regexp-in-string "/$" ""
                                                 (read-string "Directory: " (projectile-project-root)))))
    (let ((etags-command "/Applications/Emacs.app/Contents/MacOS/bin/etags")
          (etags-regex-file (expand-file-name "lisp/tags_regexfile" user-emacs-directory))
          (allowed-ext "(yml|php[0-9]?|ini|html?|js|[ch]|el)")
          (target-dir dir))
      (shell-command 
       (format "find -E %s/ -type f -regex \".*\\.%s$\" | %s -o %s --regex=@%s -"
               target-dir
               allowed-ext
               etags-command
               (expand-file-name "TAGS" target-dir)
               etags-regex-file))))
  
  (use-package etags-select)
  (use-package etags-table))

