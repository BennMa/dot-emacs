(use-package company
  :diminish (company-mode . "")
  :config
  (progn
    ;; From https://github.com/company-mode/company-mode/issues/87
    ;; See also https://github.com/company-mode/company-mode/issues/123
    (defadvice company-pseudo-tooltip-unless-just-one-frontend
        (around only-show-tooltip-when-invoked activate)
      (when (company-explicit-action-p)
        ad-do-it))
    (global-company-mode)

    (use-package company-c-headers
      :config (add-to-list 'company-backends 'company-c-headers))))

(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :bind (("C-c y TAB" . yas-expand)
         ("C-c y n"   . yas-new-snippet)
         ("C-c y s"   . yas-insert-snippet)         
         ("C-c y v"   . yas-visit-snippet-file))
  :config
  (progn
    (defun yas-new-snippet (&optional choose-instead-of-guess)
      (interactive "P")
      (let ((guessed-directories (yas-guess-snippet-directories)))
        (switch-to-buffer "*new snippet*")
        (erase-buffer)
        (kill-all-local-variables)
        (snippet-mode)
        (set (make-local-variable 'yas-guessed-modes)
             (mapcar #'(lambda (d)
                         (intern (yas-table-name (car d))))
                     guessed-directories))
        (unless (and choose-instead-of-guess
                     (not (y-or-n-p "Insert a snippet with useful headers? ")))
          (yas-expand-snippet
           (concat "\n"
                   "# -*- mode: snippet -*-\n"
                   "# name: $1\n"
                   "# --\n"
                   "$0\n")))))
    
    (yas-load-directory "~/.emacs.d/snippets/")
    (unbind-key "C-i" yas-minor-mode-map)
    ;; (bind-key "C-i" 'yas-next-field-or-maybe-expand yas-keymap)
    (yas-global-mode)))

(use-package auto-yasnippet
  :bind (("C-c y c" . aya-create)
         ("C-c y y" . aya-expand)))
