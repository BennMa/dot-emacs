;; ------ Define Constants
(defconst ROOT-DIR user-emacs-directory)

;; ------ Set basic load-path
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lisp" "themes" "")))

;; ------ package settings
(require 'cl)
(require 'package)
(eval-when-compile
  (package-initialize))
(if (null package-archive-contents)
    (package-refresh-contents))

;; ------ Basic Settings
(load (expand-file-name "conf.d/global-settings" ROOT-DIR))
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil
      ring-bell-function 'ignore
      force-load-messages nil)
(random t)

;; ------ Package Manager Initilization
(unless (require 'use-package nil t)
  (package-install 'use-package))
(require 'bind-key)

;; ------ Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; ------ Basic Hooks
;; (add-hook 'kill-emacs-hook
;;           (lambda () (byte-recompile-directory
;;                       (expand-file-name "init.d/" ROOT-DIR) 0 t)))

;; ------ Loading Packages
(use-package my-toolkit
  :config
  ;; org faces: http://orgmode.org/worg/org-color-themes.html
  (load-theme 'my-leuven t)
  (load (expand-file-name "conf.d/global-keybinding" ROOT-DIR))
  (defvar emacs-english-fonts
    '( "Anonymous Pro" "Monaco" "Inconsolata" "Ubuntu Mono"
       "Droid Sans Mono" "Menlo" "DejaVu Sans Mono" "Courier New"
       "Monospace" "Courier" ))
  (defvar emacs-chinese-fonts
    '( "宋体" "黑体" "新宋体" "文泉驿等宽微米黑" "Microsoft Yahei" ))
  (defvar emacs-font-size 14)
  (qiang-set-font emacs-english-fonts emacs-font-size emacs-chinese-fonts))

(use-package load-dir
  :config 
  (load-dir-one (expand-file-name "init.d/" ROOT-DIR)))
