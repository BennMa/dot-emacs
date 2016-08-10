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

(use-package dired
  :bind ("C-c J" . dired-double-jump)
  :ensure nil
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
  (use-package dired-x :disabled t)
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

(use-package popwin
  :config
  (popwin-mode 1))

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

(use-package hl-line
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  (use-package hl-line+))

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


(use-package bookmark
  :defer 10
  :config
  (use-package bookmark+ :ensure t))

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
  :ensure nil
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
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-c $" flyspell-mode-map))

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
  :ensure nil
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
  :disabled t
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package hexcolour
  :no-require t
  :ensure nil
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

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))


;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-e" . mc/mark-all-like-this)))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package ace-popup-menu
  :disabled t
  :ensure t
  :config
  (ace-popup-menu-mode 1))

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


;; ------ Releated to Key Guideline

(use-package discover
  :ensure t
  :disabled t
  :config
  (global-discover-mode 1))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode))
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-minibuffer)
  )


(use-package aggressive-indent
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  js2-mode-hook))
    (add-hook hook #'aggressive-indent-mode))
  ;; (global-aggressive-indent-mode 1)
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

(use-package indent-guide
  :config
  ;; (set-face-background 'indent-guide-face "dimgray")
  (indent-guide-global-mode))

(use-package dtrt-indent
  :disabled t
  :config
  (dtrt-indent-mode 1))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  )
