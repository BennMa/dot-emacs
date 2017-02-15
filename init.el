;;; init.el --- Thomas Frössman emacs init
;;; Commentary:
;;

;;; Code:

;; ------ set emacs start time
(defconst emacs-start-time (current-time))
(defun print-cost-time (title)
    (let ((elapsed (float-time (time-subtract (current-time)
                                              emacs-start-time))))
      (message "Loading %s...done (%.3fs)" title elapsed)))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(and
 (not noninteractive)
 (or (not (boundp 'emacs-version)) (string< emacs-version "24.3"))
 (warn "Use a newer version of Emacs for a full featured environment!"))

;; ------ some early settings
;; (setq-default gc-cons-threshold (* 20 1204 1204)
;;               gc-cons-percentage 0.5) ;; alloc.c
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil))
      message-log-max 16384
      visible-bell nil
      ring-bell-function 'ignore
      force-load-messages nil)

(fset 'yes-or-no-p 'y-or-n-p)

(and (fboundp 'menu-bar-mode)
     menu-bar-mode
     (not (eq system-type 'darwin))
     (menu-bar-mode -1))
(and (fboundp 'tool-bar-mode)
     tool-bar-mode
     (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode)
     scroll-bar-mode
     (scroll-bar-mode -1))

(random t)
(print-cost-time "before define constant")
;; --- define constant
;; refer https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el
(defconst user-emacs-directory
  "~/.emacs.d/"
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")
(defconst user-init-directory
  (expand-file-name "init.d/" user-emacs-directory))
(defconst user-lisp-directory
  (expand-file-name "lisp" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))
(defconst user-themes-directory
  (expand-file-name "themes" user-emacs-directory))

;; create data folder
(make-directory (expand-file-name ".data" user-emacs-directory) t)

;; ------ load-path
(mapc #'(lambda (path)
          (add-to-list 'load-path path)          
          (let ((default-directory path))
            (normal-top-level-add-subdirs-to-load-path)))
      (list
       user-themes-directory
       user-lisp-directory
       user-site-lisp-directory))
(print-cost-time "after load-path")
;; ------ package settings
(require 'package)
(setq
   package-enable-at-startup nil
   ;; package-load-list '((use-package t))
   package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("marmalade"   . "https://marmalade-repo.org/packages/")
     ("org"         . "http://orgmode.org/elpa/")
     ("gnu"         . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar my-log-verbose t)
  (if my-log-verbose
      (setq byte-compile-verbose t)
    (setq ad-redefinition-action 'accept))
  (setq use-package-verbose my-log-verbose
        use-package-debug nil
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01
        use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(print-cost-time "after use-package")
;; ------ packages
(require 'cl)
(use-package bind-key  :ensure t)
(use-package diminish  :ensure t)
(use-package session   :ensure t)
(use-package cus-edit  :ensure nil)
(use-package initsplit :ensure t)

(mapc #'(lambda (file)
          (load (expand-file-name file user-emacs-directory)))
      (list 
       "customization"
       "customization-individual"
       "customization-org"))
(print-cost-time "after customization")
(require 'my-toolkit)
(load (expand-file-name "keybinding" user-emacs-directory))

;; org faces: http://orgmode.org/worg/org-color-themes.html
  ;; (load-theme 'my-leuven t)
(load-theme 'my-custom t)
(qiang-set-font individual-english-fonts
                individual-font-size
                individual-chinese-fonts)
(print-cost-time "after theme")
;; (byte-recompile-directory user-package-settings-directory 0)
(mapc #'(lambda (file)
          (load (substring file 0 -3)))
      (directory-files user-init-directory t ".*\\.el" t))

;; ------ post initialization
(when t ;; window-system
  (print-cost-time load-file-name)

  (add-hook 'after-init-hook
            `(lambda ()
               (print-cost-time (concat ,load-file-name " [after-init]")))
            t))

;;; init.el ends here
