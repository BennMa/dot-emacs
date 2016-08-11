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
(defun load-conf (conf-name)
  "load configuration file"
  (load (expand-file-name (concat "conf.d/" conf-name) ROOT-DIR)))

(use-package my-toolkit
  :init 
  (load-conf "global-settings")
  (load-conf "individual-settings")
  
  :config
  (load-conf "global-keybinding")
  
  ;; org faces: http://orgmode.org/worg/org-color-themes.html
  (load-theme 'my-leuven t)
  ;; (load-theme 'my-custom t)
  
  (qiang-set-font individual-english-fonts individual-font-size individual-chinese-fonts))

(mapc
 #'(lambda (file)
     (load-file file))
 (directory-files (expand-file-name "init.d/" ROOT-DIR) t ".*\\.el" t))
