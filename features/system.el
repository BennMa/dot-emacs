(cond
 ((eq system-type 'darwin) ;; mac osx
  (progn
    (defvar remap-mac-key nil)
    (defvar osx-use-option-as-meta t)

    (when (and remap-mac-key (display-graphic-p))
      (setq mac-command-key-is-meta nil)
      (setq mac-command-modifier 'super)
      (when osx-use-option-as-meta
        ;; Treat option as meta
        (setq mac-option-key-is-meta t))
      (setq mac-option-modifier (if osx-use-option-as-meta 'meta nil))
      ;; Keybindings
      (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
      (global-set-key (kbd "s-v") 'yank)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-w") 'delete-window)
      (global-set-key (kbd "s-W") 'delete-frame)
      (global-set-key (kbd "s-n") 'make-frame)
      (global-set-key (kbd "s-z") 'undo-tree-undo)
      (global-set-key (kbd "s-s")
                      (lambda ()
                        (interactive)
                        (call-interactively (key-binding "\C-x\C-s"))))
      (global-set-key (kbd "s-Z") 'undo-tree-redo)
      (with-eval-after-load 'term
        (define-key term-raw-map (kbd "s-v") 'term-paste)))

    (use-package osx-trash
      :config (setq delete-by-moving-to-trash t))
    (use-package pbcopy)
    (use-package reveal-in-osx-finder
      :bind (("C-S-o" . reveal-in-osx-finder)))

    (defvar my--english-fonts '("Inconsolata" "Source Code Pro" "Anonymous Pro" "Monaco"
                                    "Ubuntu Mono" "Droid Sans Mono"
                                    "Menlo" "DejaVu Sans Mono" "Courier New"
                                    "Monospace" "Courier" "Iosevka Light"))
    (defvar my--chinese-fonts '("宋体" "黑体" "新宋体" "文泉驿等宽微米黑"
                                    "Microsoft Yahei"))
    (defvar my--font-size (if (eq system-type 'darwin) 14 11))

    (qiang-set-font my--english-fonts my--font-size my--chinese-fonts)
    ))

 ((eq system-type 'gnu/linux) ;; linux
  (progn
    (use-package cua
      :disabled t
      :config (cua-mode 1))
    (setq browse-url-browser-function 'browse-url-chromium)
    (setenv "PATH" "/home/benn/.config/composer/vendor/bin:/home/benn/.phpbrew/php/php-5.6.28/bin:/home/benn/.phpbrew/bin:/home/benn/.pyenv/plugins/pyenv-virtualenv/shims:/home/benn/.pyenv/shims:/home/benn/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/usr/lib/jvm/java-8-oracle/bin:/usr/lib/jvm/java-8-oracle/db/bin:/usr/lib/jvm/java-8-oracle/jre/bin")
    ;; (let ((my--exec-paths
    ;;        '("/home/benn/.phpbrew/php/php-5.6.28/bin/")))
    ;;   ;; (setenv "PATH" (concat (mapconcat 'identity my--exec-paths ";") ";" (getenv "PATH")))
    ;;   (mapc #'(lambda (path)
    ;;             (add-to-list 'exec-path path))
    ;;         my--exec-paths))
    )))
