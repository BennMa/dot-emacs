;;; load-path.el

;; refer https://github.com/thomasf/dotfiles-thomasf-emacs/blob/master/emacs.d/init.el

(defconst user-emacs-directory
  "~/.emacs.d/"
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")

(defconst user-data-directory
  (expand-file-name ".data" user-emacs-directory))
(defconst user-cache-directory
  (expand-file-name ".cache" user-emacs-directory))
(defconst user-lisp-directory
  (expand-file-name "lisp" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))
(defconst user-themes-directory
  (expand-file-name "themes" user-emacs-directory))
(defconst user-notes-directory
  (file-truename "~/notes"))

(make-directory user-data-directory t)
(make-directory user-cache-directory t)

;; emacs23 compat
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path user-themes-directory)
  (add-to-list 'load-path user-themes-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

(defun load-path-load-path ()
  (let ((load-path load-path))
    (require 'package)
    (package-initialize)
    ;; Add top-level lisp directories, in case they were not setup by the
    ;; environment.
    (dolist (dir (nreverse
                  (list user-lisp-directory
                        user-site-lisp-directory)))
      (dolist (entry (nreverse (directory-files-and-attributes dir)))
        (and
         (cadr entry)
         (not (string= (car entry) ".."))
         (add-to-load-path (car entry) dir))))

    ;; (mapc #'add-to-load-path
    ;;       (nreverse
    ;;        (list
    ;;         "/usr/local/share/emacs/site-lisp/mu4e/")))
    
    (delete-dups
     (delq nil (mapcar #'(lambda (x)
                         (if (file-directory-p x)
                             x
                           nil))
                     load-path)))))


(defmacro load-path-set-load-path ()
  `(progn
     (setq load-path ',(load-path-load-path))
     (let ((failed nil))
       (mapc #'(lambda (x)
                 (unless failed
                   (setq failed (not (file-directory-p x)))))
             load-path)
       (when failed
         (require 'bytecomp)
         (let ((byte-compile-verbose nil)
               (byte-compile-warnings nil)
               (use-package-verbose nil)
               (ad-redefinition-action 'accept))
           (byte-recompile-file (expand-file-name "load-path.el" user-emacs-directory) t 0 t))))))

(load-path-set-load-path)

(provide 'load-path)

;;; load-path.el ends here
