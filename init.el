;; ------ Define Constants
(defconst my-package-init-dir    (expand-file-name "init.d/" user-emacs-directory))
(defconst my-custom-settings-dir (expand-file-name "conf.d/"
                                                   user-emacs-directory))

;; ------ set load path
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lisp" "themes" "")))

;; ------ Basic Settings
(load (expand-file-name "custom-settings" my-custom-settings-dir))
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil
      ring-bell-function 'ignore)

;; ------ Package Manager Settings

(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)

;; (add-hook 'kill-emacs-hook (lambda () (byte-recompile-directory my-init-dir 0
;; t)))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

(require-package 'use-package)
(require-package 'req-package)
;; (req-package-force el-get
;;   :load-path "el-get/el-get/"
;;   :ensure t
;;   :init
;;   (add-to-list 'el-get-recipe-path (expand-file-name "el-get/el-get/recipes" user-emacs-directory))
;;   (el-get 'sync))

;; ------ Loading
(random t)
(req-package-force load-dir
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config

  ;; ------ Loading Settings
  (load (expand-file-name "my-toolkit" user-emacs-directory))
  (load (expand-file-name "global-keybinding" my-custom-settings-dir))

  ;; ------ Loading Packages
  (load-dir-one my-package-init-dir)

  (req-package-finish))
