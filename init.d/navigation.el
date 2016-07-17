(use-package helm
  ;; :demand t
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ;; ("C-x f"   . helm-multi-files)
         ("M-H"     . helm-resume)
         ("M-r"     . helm-mini)
         ("M-i"     . helm-semantic-or-imenu)
         ("M-s M-o" . helm-occur))
  
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  (unbind-key "M-v" helm-map)
  (bind-key "C-M-v" 'helm-previous-page helm-map)
  (bind-key "C-S-v" 'helm-scroll-other-window helm-map)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package helm-gtags
  :config
  (add-hook 'projectile-mode-hook #'(lambda ()
                                      (helm-gtags-mode 1)
                                      (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))
  
  (bind-keys :map helm-gtags-mode-map
             ("C-c g a" . helm-gtags-tags-in-this-function)
             ("C-c g c" . helm-gtags-create-tags)
             ("C-c g u" . helm-gtags-update-tags)
             ("C-c g r" . helm-gtags-find-rtags)
             ("C-c g s" . helm-gtags-find-symbol)
             ("C-c g h" . helm-gtags-show-stack)
             ("M-." . helm-gtags-dwim)
             ("M-," . helm-gtags-pop-stack)
             ("M-?" . helm-gtags-select)
             ("C-c <" . helm-gtags-previous-history)
             ("C-c >" . helm-gtags-next-history)))


(use-package ggtags
  :disabled t
  :config

  (add-hook 'projectile-mode-hook 'ggtags-mode)

  (bind-keys :map ggtags-mode-map
             ("C-c g s" . ggtags-find-other-symbol)
             ("C-c g h" . ggtags-view-tag-history)
             ("C-c g r" . ggtags-find-reference)
             ("C-c g f" . ggtags-find-file)
             ("C-c g c" . ggtags-create-tags)
             ("C-c g u" . ggtags-update-tags)
             ("M-," . pop-tag-mark)))


(use-package imenu
  :config
  (use-package imenu+))

(use-package imenu-list
  :bind ("C-. i" . imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

(use-package imenu-anywhere
  :disabled t
  :init
  (setq imenu-anywhere-delimiter " / "))

(use-package ag
  :commands (ag ag-regexp))

(use-package helm-ag
  :commands helm-ag)

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
     (t (format "%8d" (buffer-size))))))

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
                filename-and-process))))

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
  (add-hook 'ido-minibuffer-setup-hook
            #'(lambda ()
                (bind-key "<return>" 'ido-smart-select-text
                          ido-file-completion-map))))

(use-package ido-hacks
  :disabled t
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

(use-package smex
  :bind (("M-x" . smex)
         ("C-c M-x" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))


(use-package window-number
  :config
  (unbind-key "C-x C-j" window-number-mode-map)
  (window-number-mode)
  (window-number-meta-mode)
  (custom-set-faces '(window-number-face ((t nil)) t)))


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


(use-package isearch
  :no-require t
  :ensure nil
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
    (call-interactively 'isearch-forward)))

(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind ("M-s O" . moccur))

(use-package ace-jump-mode
  :bind (("M-j" . ace-jump-mode)))

(use-package sr-speedbar
  :bind (("C-. s" . sr-speedbar-toggle)
         :map speedbar-key-map
         ("<tab>" . speedbar-toggle-line-expansion)
         ("q" . sr-speedbar-close)))

(use-package function-args
  :disabled t
  :config
  (unbind-key "M-i" function-args-mode-map)
  (unbind-key "M-u" function-args-mode-map)
  
  (fa-config-default))
