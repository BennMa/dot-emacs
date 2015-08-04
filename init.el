(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

;; ------ set load path
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "site-lisp/el-get" "site-lisp/use-package" "lisp" "")))

;; ------ load customization settings
(load (expand-file-name "custom-settings" user-emacs-directory))

;; ------ core packages
(eval-and-compile
  (require 'cl))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle gnus)

(el-get-bundle use-package)
(eval-and-compile
  (defvar use-package-verbose t)
  (require 'use-package))
(require 'bind-key)
(require 'diminish nil t)

;; ------ packages from elpa
(el-get-bundle elpa:projectile)
(el-get-bundle elpa:helm)
(el-get-bundle elpa:helm-projectile)
(el-get-bundle elpa:ido-hacks)
(el-get-bundle elpa:flx-ido)
(el-get-bundle elpa:magit)
(el-get-bundle elpa:web-mode)
(el-get-bundle elpa:color-theme)
(el-get-bundle elpa:powerline)
(el-get-bundle elpa:window-number)
(el-get-bundle elpa:auto-highlight-symbol)

;; ------ packages from others
(el-get-bundle ag)
(el-get-bundle helm-ag)

;; ------ utility macros and functions
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defun list-regex-match-p (string list)
  (catch 'matched_ (dolist (regex list)
                     (if (string-match regex string)
                         (throw 'matched_ t))) nil))

;; ------ enable disabled commands
(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

;; ------ keybindings init
(load (expand-file-name "keybindings-init" user-emacs-directory))

;; ------ packages init
(load (expand-file-name "packages-init" user-emacs-directory))

;; ------ time costing
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; ====== init.el ends here
