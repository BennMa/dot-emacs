(progn
  ;; org faces: http://orgmode.org/worg/org-color-themes.html
  ;; (load-theme 'my-leuven t)
  ;; (load-theme 'my-custom t)

  ;; ;; https://github.com/bbatsov/zenburn-emacs
  
  (setq zenburn-override-colors-alist
      '(("zenburn-bg+05" . "#282828")
        ("zenburn-bg+1"  . "#2F2F2F")
        ("zenburn-bg+2"  . "#3F3F3F")
        ("zenburn-bg+3"  . "#4F4F4F")))
  ;; use variable-pitch fonts for some headings and titles
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t)
  ;; (use-package zenburn-theme)
  (load-theme 'zenburn t)

  ;; (use-package tao-theme
  ;;   :config
  ;;   (load-theme 'tao-yang t))

  ;; (use-package atom-one-dark-theme
  ;;   :config
  ;;   (load-theme 'atom-one-dark t))

  ;; ;; https://github.com/hlissner/emacs-doom-theme
  ;; (use-package doom-themes
  ;;   :config
  ;;   (progn
  ;;     (setq doom-enable-italic t
  ;;           doom-enable-bold t
  ;;           doom-enable-brighter-comments t)
  ;;     ;; (load-theme 'doom-molokai t)
  ;;     ;; (load-theme 'doom-nord-light t)
  ;;     ;; (load-theme 'doom-nord t)
  ;;     (load-theme 'doom-peacock t)
  ;;     (custom-set-faces '(font-lock-comment-face
  ;;                         ((t (:inherit font-lock-comment-face :slant italic)))))))

  ;; (add-hook 'text-mode-hook (lambda () (variable-pitch-mode 1)))
  ;; (require 'poet-dark-theme)
  ;; (use-package olivetti
  ;;     :config
  ;;     (add-hook 'after-init-hook (lambda ()
  ;;                                  (add-hook 'org-mode-hook (lambda () (olivetti-mode 1)))
  ;;                                  (add-hook 'markdown-mode-hook (lambda () (olivetti-mode 1))))))
  )
