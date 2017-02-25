(general-define-key (general-chord "zz") 'hydra-zoom/body)

;; ------ packages
(use-package maxframe
  :if window-system
  :commands maximize-frame 
  :bind (("C-c M" . emacs-max)
         ("C-c m" . emacs-toggle-size))
  :config
  (progn
    (defvar emacs-min-top 23)
    (defvar emacs-min-left 0)
    (defvar emacs-min-width 85)
    (defvar emacs-min-height 35)    
    (defun emacs-min ()
      (interactive)
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top emacs-min-top)
      (set-frame-parameter (selected-frame) 'left emacs-min-left)
      (set-frame-parameter (selected-frame) 'height emacs-min-height)
      (set-frame-parameter (selected-frame) 'width emacs-min-width))    
    (defun emacs-max ()
      (interactive)
      (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))    
    (defun emacs-toggle-size ()
      (interactive)
      (if (> (cdr (assq 'width (frame-parameters))) 100)
          (emacs-min)
        (maximize-frame)))))

(use-package popwin :disabled t :config (popwin-mode 1))
;; Enforce rules for popup windows
;; https://github.com/wasamasa/shackle
(use-package shackle :config (shackle-mode))

(use-package escreen
  ;; :bind-keymap ("C-c w" . escreen-map)
  :commands (escreen-create-screen)
  :config
  (bind-key "e" 'escreen-goto-last-screen escreen-map)
  (bind-key "m" 'escreen-menu escreen-map)
  (escreen-install))

(use-package zoom-frm :ensure t
  :commands (zoom-frm-in
             zoom-frm-out
             zoom-frm-unzoom
             zoom-in
             zoom-out
             hydra-zoom/body)
  :config
  (setq zoom-frame/buffer 'buffer)

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
    ("q" nil :color blue)))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package multi-term :ensure t
  :bind (("M-t" . my-term-toggle)
         ("C-M-t" . multi-term))
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
                   (if (and (string= term-ansi-at-host "localhost") ;; (string= term-ansi-at-host (system-name))
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

    (defun my-term-toggle ()
      "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
      (interactive)
      (let ((b (my-term-last-buffer (buffer-list))))
        (if (eq 'term-mode major-mode)
            (let ((blaine--buffername-whitelist
                   (remove "\\*terminal" blaine--buffername-whitelist)))
              (blaine/last-buffer))
          (if (not b)
              (multi-term)
            (switch-to-buffer b)))))

    (defun my-term-last-buffer (l)
      "Return most recently used term buffer."
      (when l
        (if (eq 'term-mode (with-current-buffer (car l) major-mode))
            (car l) (my-term-last-buffer (cdr l)))))

    ;; (defun my-term-clear ()
    ;;   (interactive)
    ;;   (let ((comint-buffer-maximum-size 0))
    ;;     (comint-truncate-buffer)))

    (add-hook 'term-mode-hook
              #'(lambda ()
                  ;; (projectile-mode -1)
                  (company-mode -1)
                  ;; (auto-highlight-symbol-mode -1)
                  (yas-minor-mode -1)))))

(use-package paradox :commands paradox-list-packages)

(use-package dired :ensure nil
  :config
  (progn
    (use-package dired-k
      :commands (dired-k direx-k dired-k-no-revert)
      :init
      (progn
        ;; always execute dired-k when dired buffer is opened
        (add-hook 'dired-initial-position-hook 'dired-k)
        (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))
    ))

(use-package direx
  :bind ("C-c d" . my-direx:jump-to-directory-other-window)
  :config
  (progn
    ;; (add-hook 'direx:direx-mode-hook 'direx-k)
    ;; (push '(direx:direx-mode :position left :width 30 :dedicated t :stick t)
    ;;       popwin:special-display-config)
    (push '(direx:direx-mode :align left :select t :size 30) shackle-rules)
    :config
    (defun my-direx:jump-to-directory-other-window ()
      (interactive)
      ;; (switch-to-buffer-other-window (direx:jump-to-directory-noselect))
      (direx:jump-to-directory-other-window)
      (set-window-dedicated-p (selected-window) t))

    (bind-key "TAB" 'direx:maybe-find-item direx:direx-mode-map)
    (defadvice direx:jump-to-directory-noselect
        (around direx:set-default-directory activate)
      (let ((default-directory (projectile-project-root)))
        ad-do-it))

    (defun direx:do-rename-file ()
      (interactive)
      (let* ((item (direx:item-at-point!))
             (file (direx:item-tree item))
             (to (read-file-name (format "Rename %s to " (direx:tree-name file))
                                 (direx:directory-dirname (direx:file-full-name file)))))
        (dired-rename-file (direx:file-full-name file) to nil)
        (direx:item-refresh-parent item)
        (direx:move-to-item-name-part item)))

    (defun direx:do-copy-files ()
      (interactive)
      (let* ((item (direx:item-at-point!))
             (file (direx:item-tree item))
             (to (read-directory-name (format "Copy %s to " (direx:tree-name file))
                                      (direx:directory-dirname (direx:file-full-name file)))))
        (dired-copy-file (direx:file-full-name file) to nil)
        (direx:item-refresh-parent item)
        (direx:move-to-item-name-part item)))))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-log-buffer-file))
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
