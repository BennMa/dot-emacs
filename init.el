;; ------ Define Constants
(defconst ROOT-DIR user-emacs-directory)

;; ------ Set basic load-path
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lisp" "themes" "")))

;; ------ Basic Settings
(load (expand-file-name "conf.d/global-settings" ROOT-DIR))
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil
      ring-bell-function 'ignore
      force-load-messages nil)
(random t)

;; ------ Package Manager Settings
(require 'cl)
(require 'package)
(eval-when-compile
  (package-initialize))
(if (null package-archive-contents)
    (package-refresh-contents))

(unless (require 'use-package nil t)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(use-package req-package :ensure t)
(use-package el-get      :ensure t)

;; ------ Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; ------ Basic Hooks
(add-hook 'kill-emacs-hook
          (lambda () (byte-recompile-directory
                      (expand-file-name "init.d/" ROOT-DIR) 0 t)))

;; ------ Loading Packages
(req-package-force my-toolkit
  :config
  (load (expand-file-name "conf.d/global-keybinding" ROOT-DIR)))
(req-package-force load-dir
  :config 
  (load-dir-one (expand-file-name "init.d/" ROOT-DIR))
  (req-package-finish))
