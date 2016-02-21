;; org faces: http://orgmode.org/worg/org-color-themes.html
(use-package custom-theme
  :no-require t
  :init
  (load-theme 'my-leuven t)

  (defvar emacs-english-fonts  '( "Anonymous Pro" "Monaco" "Inconsolata" "Ubuntu Mono"
                                  "Droid Sans Mono" "Menlo" "DejaVu Sans Mono" "Courier New"
                                  "Monospace" "Courier" ))
  (defvar emacs-chinese-fonts '( "宋体" "黑体" "新宋体" "文泉驿等宽微米黑"
                                 "Microsoft Yahei" ))
  (defvar emacs-font-size 14)
  (qiang-set-font emacs-english-fonts 14 emacs-chinese-fonts))

(use-package maxframe
  :if window-system
  :commands maximize-frame
  :bind (("C-c M" . emacs-max)
         ("C-c m" . emacs-toggle-size))
  :init
  ;; (add-hook 'after-init-hook 'maximize-frame)
  ;; (add-hook 'after-init-hook 'emacs-min)
  
  :config
  (defvar emacs-min-top 23)
  (defvar emacs-min-left 0)
  (defvar emacs-min-width 85)
  (defvar emacs-min-height 35)

  ;; (let ((frame-alist
  ;;        (list (cons 'top    emacs-min-top)
  ;;              (cons 'left   emacs-min-left)
  ;;              (cons 'height emacs-min-height)
  ;;              (cons 'width  emacs-min-width))))
  ;;   (setq initial-frame-alist frame-alist))
  
  (defun emacs-min ()
    (interactive)
    (set-frame-parameter (selected-frame) 'fullscreen nil)
    (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
    (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

    (set-frame-parameter (selected-frame) 'top emacs-min-top)
    (set-frame-parameter (selected-frame) 'left emacs-min-left)
    (set-frame-parameter (selected-frame) 'height emacs-min-height)
    (set-frame-parameter (selected-frame) 'width emacs-min-width))
  
  ;; (qiang-set-font emacs-english-fonts 15 emacs-chinese-fonts)
  
  (defun emacs-max ()
    (interactive)
    (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
    (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
    (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
  
  (defun emacs-toggle-size ()
    (interactive)
    (if (> (cdr (assq 'width (frame-parameters))) 100)
        (emacs-min)
      (maximize-frame))))

(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode)

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

(use-package helm-config
  :demand t
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ;; ("C-x f"   . helm-multi-files)
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
  (unbind-key "M-v" helm-map)
  (bind-key "C-M-v" 'helm-previous-page helm-map)
  (bind-key "C-S-v" 'helm-scroll-other-window helm-map)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package company
  :diminish company-mode
  ;; :commands company-mode
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  (global-company-mode))

(use-package projectile
  :demand t
  :diminish projectile-mode
  :commands projectile-global-mode
  :bind-keymap ("M-p" . projectile-command-map)
  :config
  (unbind-key "C-c p" projectile-mode-map)

  (use-package symfony1x
    :load-path "lisp/symfony1x"
    :commands symfony1x-mode
    :init
    (setq symfony1x-mode-key-prefix "C-; ;"))

  (add-hook 'projectile-mode-hook
            #'(lambda ()
                (when (and (buffer-file-name)
                           (string-match "\\/Master_\\(?:Service\\|Beta\\|Community\\|FT\\)\\/" (buffer-file-name)))
                  (make-local-variable 'symfony1x-mode-status)
                  (symfony1x-mode t))
                (ggtags-mode 1)
                (auto-highlight-symbol-mode 1)))

  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))
  
  (projectile-global-mode))

(use-package git-messenger
  :bind ("C-x G" . git-messenger:popup-message))


(use-package magit
  :bind (("C-x g" . magit-status))
  ;; ("C-x G" . magit-status-with-prefix))
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
  :bind (("C-x C-b" . my-ibuffer-startup)
         ("C-x b" . my-ibuffer-startup))
  :config
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-auto-mode 1)
                (ibuffer-switch-to-saved-filter-groups "default")))

  (defun my-ibuffer-startup ()
    "Open ibuffer with cursour pointed to most recent buffer name"
    (interactive)
    (let ((recent-buffer-name (buffer-name)))
      (ibuffer)
      (ibuffer-jump-to-buffer recent-buffer-name)))

  (defun my-ibuffer-never-show-predicates (buffer)
    (let ((name (buffer-name buffer)))
      (and (not (or (member name '("*scratch*", "*Messages*"))
                    (string-match-p "^\\*terminal" name)))
           (or (string-match-p "^ ?\\*" name)
               (string-match-p "^TAGS" name)))))

  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (use-package ibuffer-vc
    :config
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))

    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)))))

(use-package ido
  :demand t
  :defines (ido-cur-item
            ido-require-match
            ido-selected
            ido-final-text
            ido-show-confirm-message)
  :bind (("C-M-<tab>" . ido-switch-buffer)
         ;; ("C-x b" . ido-switch-buffer)
         ("C-x C-f" . ido-find-file)
         ("C-x f" . ido-find-file))
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


(use-package powerline
  :config
  (powerline-default-theme))

(use-package window-number
  :config
  (unbind-key "C-x C-j" window-number-mode-map)
  (window-number-mode)
  (window-number-meta-mode)
  (custom-set-faces '(window-number-face ((t nil)) t)))

(use-package cus-edit
  :defer 5
  :config
  (use-package initsplit))

(use-package fetchmail-mode
  :commands fetchmail-mode
  :mode ("fetchmailrc" . fetchmail-mode))

(use-package nf-procmail-mode
  :commands nf-procmail-mode
  :mode ("procmailrc" . nf-procmail-mode))

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
                                nil nil "~/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "~/")))
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
  (unbind-key "M-{" dired-mode-map)
  (unbind-key "M-}" dired-mode-map)

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
  :disabled t
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

(use-package direx
  :bind ("C-. d" . my-direx:jump-to-directory-other-window)
  :init
  (push '(direx:direx-mode :position left :width 30 :dedicated t :stick t)
        popwin:special-display-config)
  :config
  (defun my-direx:jump-to-directory-other-window ()
    (interactive)
    (switch-to-buffer-other-window (direx:jump-to-directory-noselect))
    (set-window-dedicated-p (selected-window) t))
  
  (bind-key "TAB" 'direx:maybe-find-item direx:direx-mode-map)
  (defadvice direx:jump-to-directory-noselect
      (around direx:set-default-directory activate)
    (let ((default-directory (projectile-project-root)))
      ad-do-it)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
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
  (use-package php-doc)
  
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (local-set-key (kbd "M-D") 'php-insert-doc-block))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

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

(use-package hl-line
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  (use-package hl-line+))

(use-package etags
  :disabled t
  :bind* ("M-." . etags-select-find-tag-at-point)
  :bind  ("M-?" . etags-select-find-tag)
  :config
  ;; (bind-key* "M-p R" 'my-regenerate-tags)
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

(use-package session
  :if (not noninteractive)
  :preface
  (defun remove-session-use-package-from-settings ()
    (when (string= (file-name-nondirectory (buffer-file-name))
                   "custom-settings.el")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^ '(session-use-package " nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))))))

  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
        (show-subtree))))

  (defvar server-process nil)

  (defun save-information ()
    (with-temp-message "Saving Emacs information..."
      (recentf-cleanup)

      (loop for func in kill-emacs-hook
            unless (memq func '(exit-gnus-on-exit server-force-stop))
            do (funcall func))

      (unless (or noninteractive
                  (and server-process
                       (eq 'listen (process-status server-process))))
        (server-start))))

  :config
  (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
  (add-hook 'session-after-jump-to-last-change-hook 'le::maybe-reveal)
  (run-with-idle-timer 60 t 'save-information)
  (add-hook 'after-init-hook 'session-initialize t))


(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))

(use-package hilit-chg
  :bind ("M-o C" . highlight-changes-mode))

(use-package hippie-exp
  :bind (("M-/" . dabbrev-expand)
         ("M-?" . hippie-expand))
  :preface
  (autoload 'yas-expand "yasnippet" nil t)

  (defun my-yas-hippie-try-expand (first-time)
    (if (not first-time)
        (let ((yas-fallback-behavior 'return-nil))
          (yas-expand))
      (undo 1)
      nil))

  (defun my-hippie-expand-completions (&optional hippie-expand-function)
    "Return the full list of possible completions generated by `hippie-expand'.
   The optional argument can be generated with `make-hippie-expand-function'."
    (let ((this-command 'my-hippie-expand-completions)
          (last-command last-command)
          (buffer-modified (buffer-modified-p))
          (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
      (flet ((ding))        ; avoid the (ding) when hippie-expand exhausts its
                                        ; options.
        (while (progn
                 (funcall hippie-expand-function nil)
                 (setq last-command 'my-hippie-expand-completions)
                 (not (equal he-num -1)))))
      ;; Evaluating the completions modifies the buffer, however we will finish
      ;; up in the same state that we began.
      (set-buffer-modified-p buffer-modified)
      ;; Provide the options in the order in which they are normally generated.
      (delete he-search-string (reverse he-tried-table))))

  (defmacro my-ido-hippie-expand-with (hippie-expand-function)
    "Generate an interactively-callable function that offers ido-based
  completion using the specified hippie-expand function."
    `(call-interactively
      (lambda (&optional selection)
        (interactive
         (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
           (if options
               (list
                ;; (ido-completing-read "Completions: " options)
                (completing-read "Completions: " options)
                ))))
        (if selection
            (he-substitute-string selection t)
          (message "No expansion found")))))

  (defun my-ido-hippie-expand ()
    "Offer ido-based completion for the word at point."
    (interactive)
    (my-ido-hippie-expand-with 'hippie-expand))

  (defun my-try-expand-company (old)
    (require 'company)
    (unless company-candidates
      (company-auto-begin))
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     company-candidates))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-tag-beg ()
    (save-excursion
      (backward-word 1)
      (point)))

  (defun tags-complete-tag (string predicate what)
    (save-excursion
      ;; If we need to ask for the tag table, allow that.
      (if (eq what t)
          (all-completions string (tags-completion-table) predicate)
        (try-completion string (tags-completion-table) predicate))))

  (defun try-expand-tag (old)
    (when tags-table-list
      (unless old
        (he-init-string (he-tag-beg) (point))
        (setq he-expand-list
              (sort (all-completions he-search-string 'tags-complete-tag)
                    'string-lessp)))
      (while (and he-expand-list
                  (he-string-member (car he-expand-list) he-tried-table))
        (setq he-expand-list (cdr he-expand-list)))
      (if (null he-expand-list)
          (progn
            (when old (he-reset-string))
            ())
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)))

  (defun my-dabbrev-substring-search (pattern &optional reverse limit)
    (let ((result ())
          (regpat (cond ((not hippie-expand-dabbrev-as-symbol)
                         (concat (regexp-quote pattern) "\\sw+"))
                        ((eq (char-syntax (aref pattern 0)) ?_)
                         (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
                        (t
                         (concat (regexp-quote pattern)
                                 "\\(\\sw\\|\\s_\\)+")))))
      (while (and (not result)
                  (if reverse
                      (re-search-backward regpat limit t)
                    (re-search-forward regpat limit t)))
        (setq result (buffer-substring-no-properties
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (skip-syntax-backward "w_")
                        (point))
                      (match-end 0)))
        (if (he-string-member result he-tried-table t)
            (setq result nil)))     ; ignore if bad prefix or already in table
      result))

  (defun try-my-dabbrev-substring (old)
    (let ((old-fun (symbol-function 'he-dabbrev-search)))
      (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
      (unwind-protect
          (try-expand-dabbrev old)
        (fset 'he-dabbrev-search old-fun))))

  (defun try-expand-flexible-abbrev (old)
    "Try to complete word using flexible matching.
  Flexible matching works by taking the search string and then
  interspersing it with a regexp for any character. So, if you try
  to do a flexible match for `foo' it will match the word
  `findOtherOtter' but also `fixTheBoringOrange' and
  `ifthisisboringstopreadingnow'.
  The argument OLD has to be nil the first call of this function, and t
  for subsequent calls (for further possible completions of the same
  string).  It returns t if a new completion is found, nil otherwise."
    (if (not old)
        (progn
          (he-init-string (he-lisp-symbol-beg) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (setq he-expand-list
                (and (not (equal he-search-string ""))
                     (he-flexible-abbrev-collect he-search-string)))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          ())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

  (defun he-flexible-abbrev-collect (str)
    "Find and collect all words that flex-matches STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (let ((collection nil)
          (regexp (he-flexible-abbrev-create-regexp str)))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t)
          ;; Is there a better or quicker way than using `thing-at-point'
          ;; here?
          (setq collection (cons (thing-at-point 'word) collection))))
      collection))

  (defun he-flexible-abbrev-create-regexp (str)
    "Generate regexp for flexible matching of STR.
  See docstring for `try-expand-flexible-abbrev' for information
  about what flexible matching means in this context."
    (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
            "\\w*" "\\b"))

  (defun my-try-expand-dabbrev-visible (old)
    (save-excursion (try-expand-dabbrev-visible old)))

  :config
  (setq hippie-expand-try-functions-list
        '(my-yas-hippie-try-expand
          my-try-expand-company
          try-my-dabbrev-substring
          my-try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-tag
          try-expand-flexible-abbrev
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-line-all-buffers
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  (bind-key "M-i" 'my-ido-hippie-expand)

  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (equal (substring str -1) ")"))
        (progn (backward-delete-char 1) (forward-char)))))


(use-package backup-each-save
  :commands backup-each-save
  :preface
  (defun show-backups ()
    (interactive)
    (require 'find-dired)
    (let* ((file (substring (make-backup-file-name (buffer-file-name)) 0 -1))
           (dir (file-name-directory file))
           (args (concat "-iname '" (file-name-nondirectory file)
                         ".~*~'"))
           (dired-buffers dired-buffers)
           (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
      ;; Check that it's really a directory.
      (or (file-directory-p dir)
          (error "Backup directory does not exist: %s" dir))
      (with-current-buffer (get-buffer-create "*Backups*")
        (let ((find (get-buffer-process (current-buffer))))
          (when find
            (if (or (not (eq (process-status find) 'run))
                    (yes-or-no-p "A `find' process is running; kill it? "))
                (condition-case nil
                    (progn
                      (interrupt-process find)
                      (sit-for 1)
                      (delete-process find))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))

        (widen)
        (kill-all-local-variables)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq default-directory dir
              args (concat
                    find-program " . "
                    (if (string= args "")
                        ""
                      (concat
                       (shell-quote-argument "(")
                       " " args " "
                       (shell-quote-argument ")")
                       " "))
                    (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                      (car find-ls-option))
                        (format "%s %s %s"
                                (match-string 1 (car find-ls-option))
                                (shell-quote-argument "{}")
                                find-exec-terminator)
                      (car find-ls-option))))
        ;; Start the find process.
        (message "Looking for backup files...")
        (shell-command (concat args "&") (current-buffer))
        ;; The next statement will bomb in classic dired (no optional arg
        ;; allowed)
        (dired-mode dir (cdr find-ls-option))
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map "\C-c\C-k" 'kill-find)
          (use-local-map map))
        (make-local-variable 'dired-sort-inhibit)
        (setq dired-sort-inhibit t)
        (set (make-local-variable 'revert-buffer-function)
             `(lambda (ignore-auto noconfirm)
                (find-dired ,dir ,find-args)))
        ;; Set subdir-alist so that Tree Dired will work:
        (if (fboundp 'dired-simple-subdir-alist)
            ;; will work even with nested dired format (dired-nstd.el,v 1.15
            ;; and later)
            (dired-simple-subdir-alist)
          ;; else we have an ancient tree dired (or classic dired, where
          ;; this does no harm)
          (set (make-local-variable 'dired-subdir-alist)
               (list (cons default-directory (point-min-marker)))))
        (set (make-local-variable 'dired-subdir-switches)
             find-ls-subdir-switches)
        (setq buffer-read-only nil)
        ;; Subdir headlerline must come first because the first marker in
        ;; subdir-alist points there.
        (insert "  " dir ":\n")
        ;; Make second line a ``find'' line in analogy to the ``total'' or
        ;; ``wildcard'' line.
        (insert "  " args "\n")
        (setq buffer-read-only t)
        (let ((proc (get-buffer-process (current-buffer))))
          (set-process-filter proc (function find-dired-filter))
          (set-process-sentinel proc (function find-dired-sentinel))
          ;; Initialize the process marker; it is used by the filter.
          (move-marker (process-mark proc) 1 (current-buffer)))
        (setq mode-line-process '(":%s")))))

  (bind-key "C-x ~" 'show-backups)

  :init
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (file-truename file)))

  (add-hook 'after-save-hook 'backup-each-save)

  :config
  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (setq backup-each-save-filter-function 'backup-each-save-filter)

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  (setq backup-enable-predicate 'my-dont-backup-files-p))

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
  (unbind-key "C-." flyspell-mode-map))

(use-package flycheck
  :demand t
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (flycheck-add-mode 'php 'web-mode)
  (flycheck-add-mode 'php-phpmd 'web-mode)
  (flycheck-add-mode 'php-phpcs 'web-mode)

  (defalias 'flycheck-show-error-at-point-soon
    'flycheck-show-error-at-point)
  
  (global-flycheck-mode))

(use-package diff-mode
  :commands diff-mode
  :config
  (add-hook 'diff-mode-hook 'smerge-mode))

(use-package smerge-mode
  :commands smerge-mode
  :config
  (setq smerge-command-prefix (kbd "C-. C-.")))

(use-package ediff
  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" 'ctl-period-equals-map)

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))
  
  :config
  (use-package ediff-keep))

(use-package edit-var
  :disabled t
  :bind ("C-c e v" . edit-variable))

(use-package ascii
  :disabled t
  :bind ("C-c e A" . ascii-toggle)
  :commands ascii-on
  :functions ascii-off
  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))

(use-package grep
  :disabled t
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep))
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))

  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49)))

(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)
  :init
  (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  :config
  (defadvice Info-exit (after remove-info-window activate)
    "When info mode is quit, remove the window."
    (if (> (length (window-list)) 1)
        (delete-window))))

(use-package mule
  :no-require t
  :defines x-select-request-type
  :config
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
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
        (setq file (expand-file-name ".clean" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (when (and (file-exists-p file)
                 (not (file-exists-p ".noclean"))
                 (not (and buffer-file-name
                           (string-match "\\.texi\\'" buffer-file-name))))
        (add-hook 'write-contents-hooks
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)

  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 80
        whitespace-rescan-timer-time nil
        whitespace-silent t
        whitespace-style '(face trailing lines space-before-tab empty)))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package erlang
  :mode (("^\\.erlang$" . erlang-mode)
         ("\\.app$" . erlang-mode)
         ("\\.app.src$" . erlang-mode)
         ("\\.erl$" . erlang-mode)
         ("\\.es$" . erlang-mode)
         ("\\.escript$" . erlang-mode)
         ("\\.eterm$" . erlang-mode)
         ("\\.script$" . erlang-mode)
         ("\\.yaws$" . erlang-mode))
  :config
  (use-package edts-mode
    :load-path "site-lisp/edts/elisp/edts"
    :config
    ;; remove eproject hooks which will effect other develop environment, here
    ;; also can use-package eproject first, and set up the file types to nil
    (remove-hook 'find-file-hook #'eproject-maybe-turn-on)
    (remove-hook 'dired-mode-hook #'eproject-maybe-turn-on)
    (remove-hook 'after-change-major-mode-hook #'eproject--after-change-major-mode-hook)
    (remove-hook 'after-save-hook #'eproject--after-save-hook)

    (defun edts-erlang-mode-hook ()
      (when (buffer-file-name)
        (eproject-maybe-turn-on)
        (edts-mode t)))

    (add-hook 'erlang-mode-hook 'edts-erlang-mode-hook)))

(use-package hexcolour
  :no-require t
  :init
  (defvar hexcolour-keywords
    '(("#[a-zA-Z0-9]\\{6\\}"
       (0 (put-text-property (match-beginning 0)
                             (match-end 0)
                             'face (list :background 
                                         (match-string-no-properties 0)))))))
  (defun hexcolour-add-to-font-lock ()
    (font-lock-add-keywords nil hexcolour-keywords))
  (dolist (hook '(emacs-lisp-mode-hook
                  org-mode-hook
                  php-mode-hook
                  web-mode-hook
                  erlang-mode-hook))
    (add-hook hook #'(lambda() (hexcolour-add-to-font-lock)))))

(use-package eshell
  :disabled t
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (defun ss (server)
      (interactive "sServer: ")
      (call-process "spawn" nil nil nil "ss" server))

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)

  (use-package esh-toggle
    :bind (("C-x C-z" . eshell-toggle)
           ("C-. C-z" . eshell-toggle-cd))))

(use-package sh-toggle
  :disabled t
  :bind ("C-. C-z" . shell-toggle))

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))

(use-package isearch
  :no-require t
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window))
  :preface
  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward))
  )

(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind ("M-s O" . moccur))

(use-package imenu
  :config
  (use-package imenu+))

(use-package w3m
  :commands (w3m w3m-browse-url
                 my-w3m-hacknews my-w3m-reddit my-w3m-wikipedia my-w3m-open-url)
  :config
  (defun my-w3m-hacknews ()
    (interactive)
    (browse-url "http://news.ycombinator.com"))
  (defun my-w3m-reddit (reddit)
    "Opens the REDDIT in w3m-new-session"
    (interactive (list
                  (read-string "Enter the reddit (default: php): " nil nil "php" nil)))
    (browse-url (format "http://m.reddit.com/r/%s" reddit)))
  (defun my-w3m-wikipedia (search-term)
    "Search for SEARCH-TERM on wikipedia"
    (interactive
     (let ((term (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (word-at-point))))
       (list
        (read-string
         (format "Wikipedia (%s):" term) nil nil term)))
     )
    (browse-url
     (concat
      "http://en.m.wikipedia.org/w/index.php?search="
      search-term
      )))
  (defun my-w3m-open-url (url)
    "Opens site in new w3m session with 'http://' appended"
    (interactive
     (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
    (w3m-goto-url-new-session
     (concat "http://" url))))

(use-package bbdb-com
  :commands bbdb-create
  :bind ("M-B" . bbdb))

;; https://www.emacswiki.org/emacs/TabBarMode
;; https://zhangda.wordpress.com/2012/09/21/tabbar-mode-rocks-with-customization/
(use-package tabbar
  :demand t
  :bind (("M-{" . tabbar-backward-tab)
         ("M-}" . tabbar-forward-tab)
         ("C-M-{" . tabbar-backward-group)
         ("C-M-}" . tabbar-forward-group))
  :config
  (tabbar-mode)

  ;; (setq tabbar-background-color "#959A79")
  ;; (custom-set-faces
  ;;  '(tabbar-default ((t (:inherit variable-pitch :background "#959A79" :foreground "black" :weight bold))))
  ;;  '(tabbar-button ((t (:inherit tabbar-default :foreground "dark red"))))
  ;;  '(tabbar-button-highlight ((t (:inherit tabbar-default))))
  ;;  '(tabbar-highlight ((t (:underline t))))
  ;;  '(tabbar-selected ((t (:inherit tabbar-default :background "#95CA59"))))
  ;;  '(tabbar-separator ((t (:inherit tabbar-default :background "#95CA59"))))
  ;;  '(tabbar-unselected ((t (:inherit tabbar-default)))))

  (defun my-tabbar-buffer-groups ()
    "Returns the name of the tab group names the current buffer belongs to.
     This works at least with Emacs v24.2 using tabbar.el v1.7."
    (list
     (cond
      ((string-match-p "TAGS.*" (buffer-name))
       "Other")
      ((memq major-mode
             '(help-mode apropos-mode Info-mode Man-mode direx:direx-mode))
       "Other")
      ((eq major-mode 'term-mode)
       "Term")      
      ((string-match-p "\*.*\*" (buffer-name))
       "Other")
      ((eq major-mode 'org-mode)
       "Org")
      ((eq major-mode 'erc-mode)
       "ERC")
      ((eq major-mode 'dired-mode)
       "Dired")
      ((projectile-project-p)
       (concat "P: " (projectile-project-name)))      
      (t "Other"))))
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups))

  ;; (defun my-tabbar-buffer-list ()
  ;;   (remove-if
  ;;    (lambda(buffer)
  ;;      (or
  ;;       (find (aref (buffer-name buffer) 0) " *")))
  ;;    (buffer-list)))
  ;; (setq tabbar-buffer-list-function 'my-tabbar-buffer-list))


;; terminal --------------------------------------------------------------------------
(use-package multi-term
  :bind (("<f5>" . multi-term))
  :config
  (when (require 'term nil t)
    (defun term-handle-ansi-terminal-messages (message)
      (while (string-match "\eAnSiT.+\n" message)
        ;; Extract the command code and the argument.
        (let* ((start (match-beginning 0))
               (command-code (aref message (+ start 6)))
               (argument
                (save-match-data
                  (substring message
                             (+ start 8)
                             (string-match "\r?\n" message
                                           (+ start 8)))))
               ignore)
          ;; Delete this command from MESSAGE.
          (setq message (replace-match "" t t message))
          
          (cond ((= command-code ?c)
                 (setq term-ansi-at-dir argument))
                ((= command-code ?h)
                 (setq term-ansi-at-host argument))
                ((= command-code ?u)
                 (setq term-ansi-at-user argument))
                ((= command-code ?e)
                 (save-excursion
                   (find-file-other-window (expand-file-name argument))))
                ((= command-code ?x)
                 (save-excursion
                   (find-file argument)))
                ((= command-code ?g)
                 (save-excursion
                   (magit-status argument)))
                (t
                 (setq ignore t)))
          
          ;; (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
          ;;   (setq buffer-file-name
          ;;         (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
          ;;   (set-buffer-modified-p nil)
          ;;   (setq default-directory (if (string= term-ansi-at-host (system-name))
          ;;                               (concatenate 'string term-ansi-at-dir "/")
          ;;                             (format "/%s@%s:%s/" term-ansi-at-user
          ;;                                     term-ansi-at-host term-ansi-at-dir))))


          ;; (message "%s=%s=%s=%s=%s"
          ;;          term-ansi-at-host (system-name) term-ansi-at-user
          ;;          (user-real-login-name) term-ansi-at-dir)
          
          (if ignore
              nil
            (setq default-directory
                  (file-name-as-directory
                   (if (and (string= term-ansi-at-host (system-name))
                            (string= term-ansi-at-user (user-real-login-name)))
                       (expand-file-name term-ansi-at-dir)
                     (if (string= term-ansi-at-user (user-real-login-name))
                         (concat "/" term-ansi-at-host ":" term-ansi-at-dir)
                       (concat "/" term-ansi-at-user "@" term-ansi-at-host ":"
                               term-ansi-at-dir)))))

            ;; I'm not sure this is necessary,
            ;; but it's best to be on the safe side.
            (if (string= term-ansi-at-host (system-name))
                (progn
                  (setq ange-ftp-default-user term-ansi-at-save-user)
                  (setq ange-ftp-default-password term-ansi-at-save-pwd)
                  (setq ange-ftp-generate-anonymous-password term-ansi-at-save-anon))
              (setq term-ansi-at-save-user ange-ftp-default-user)
              (setq term-ansi-at-save-pwd ange-ftp-default-password)
              (setq term-ansi-at-save-anon ange-ftp-generate-anonymous-password)
              (setq ange-ftp-default-user nil)
              (setq ange-ftp-default-password nil)
              (setq ange-ftp-generate-anonymous-password nil)))

          ))
      message)

    (add-hook 'term-mode-hook
              #'(lambda ()
                  (projectile-mode -1)
                  (company-mode -1)
                  (ggtags-mode -1)
                  (auto-highlight-symbol-mode -1)))))

(use-package ace-jump-mode
  :bind ("M-j" . ace-jump-mode))

(use-package electric-pair-mode
  :no-require t
  :init
  (electric-pair-mode))

;; https://github.com/pashky/restclient.el
(use-package restclient
  :commands restclient-mode)

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-e" . mc/mark-all-like-this)))

;; https://github.com/rooney/zencoding
(use-package zencoding-mode
  :bind ("C-M-h" . zencoding-expand-line)
  :commands zencoding-mode
  :config
  (zencoding-mode)
  (unbind-key "C-j" zencoding-mode-keymap)
  (unbind-key "C-<return>" zencoding-mode-keymap))

;; ------ org-mode
(use-package org-init)

;; ------ gnus
(use-package gnus-init
  :disabled t
  :bind (("M-G"   . trigger-gnus)
         ("C-x m" . compose-mail)))
