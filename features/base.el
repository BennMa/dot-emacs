(use-package diminish  :ensure t)
(use-package bind-key  :ensure t)
(use-package general   :ensure t)
(use-package key-chord :ensure t :defer 1 :config (setq key-chord-two-keys-delay 0.2))
(use-package use-package-chords :ensure t :config (key-chord-mode 1))
(use-package server :config (unless (server-running-p) (server-start)))
(use-package restart-emacs :ensure t)
(use-package session)
(use-package hydra :ensure t)
(use-package which-key :ensure t
  :config
  (progn
    (which-key-mode)
    (which-key-setup-side-window-bottom)
    ;; (which-key-setup-minibuffer)
  ))

(use-package ivy
  :ensure t
  :demand t
  :diminish (ivy-mode . "")
  :bind ("C-c C-r" . ivy-resume)
  :config
  (progn
    (ivy-mode 1)))

(use-package swiper :ensure t
  :bind ("C-s" . swiper))

(use-package counsel :ensure t
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g"   . counsel-git)
   ("C-c j"   . counsel-git-grep)
   ("C-S-s"   . counsel-ag)
   ("C-x l"   . counsel-locate)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ;; ("<f2> u"  . counsel-unicode-char)
   ))

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "")
  :commands (projectile-global-mode projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (progn 
    (projectile-global-mode)

    ;; https://github.com/ericdanan/counsel-projectile
    (use-package counsel-projectile :config (counsel-projectile-on))
    ))
