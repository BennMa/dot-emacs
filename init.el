;;; init.el --- Thomas Frössman emacs init
;;; Commentary:
;;

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; ------ set emacs start time
(defconst emacs-start-time (current-time))

;; ------ some early settings
(and
 (not noninteractive)
 (or (not (boundp 'emacs-version)) (string< emacs-version "24.3"))
 (warn "Use a newer version of Emacs for a full featured environment!"))

(setq-default ;; alloc.c
 gc-cons-threshold (* 20 1204 1204)
 gc-cons-percentage 0.5)

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
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil
      ring-bell-function 'ignore
      force-load-messages nil)
(random t)

;; ------ load-path
(when load-file-name
  (load (expand-file-name
         "load-path" (file-name-directory load-file-name)) nil t))

;; ------ package.el settings
(eval-and-compile
  (defvar my-log-verbose nil)
  (if my-log-verbose
      (setq byte-compile-verbose t)
    (setq ad-redefinition-action 'accept))
  (setq use-package-verbose my-log-verbose
        use-package-debug nil
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01)
  
  (setq
   package-enable-at-startup nil
   package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("marmalade"   . "https://marmalade-repo.org/packages/")
     ("org"         . "http://orgmode.org/elpa/")
     ("gnu"         . "https://elpa.gnu.org/packages/")
     ))

  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))

  (defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
        t
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
        (progn
          (package-refresh-contents)
          (require-package package min-version t))))))

(defvar byte-compile-warnings nil)

(eval-when-compile
  (require 'package)
  (package-initialize t)
  (require-package 'use-package)
  (require 'use-package)
  
  (defmacro executable-find* (command)
    "Macro form of executable-find..."
    (executable-find command)))

;; ------ load packages
(require 'cl)
(use-package bind-key :ensure t)
(use-package diminish :ensure t)

(mapc #'(lambda (file)
          (load (expand-file-name file user-emacs-directory)))
      (list 
       "global-customization"
       "individual-customization"
       "org-customization"))

(require 'my-toolkit)
(require 'keybinding (expand-file-name "keybinding.el" user-emacs-directory))

;; org faces: http://orgmode.org/worg/org-color-themes.html
  ;; (load-theme 'my-leuven t)
(load-theme 'my-custom t)
(qiang-set-font individual-english-fonts
                individual-font-size
                individual-chinese-fonts)

(defconst user-package-settings-directory (expand-file-name "init.d/" user-emacs-directory))
;; (byte-recompile-directory user-package-settings-directory 0)
(mapc #'(lambda (file)
          (load (substring file 0 -3)))
      (directory-files user-package-settings-directory t ".*\\.el" t))

;;; File local vars

;; Local Variables:
;; eval: (require 'use-package)
;; End:

(provide 'init)

;;; init.el ends here
