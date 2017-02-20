(let ((use-package-always-ensure t))

  (use-package diminish)
  (use-package bind-key)
  (use-package general)
  (use-package key-chord :defer 1 :config (setq key-chord-two-keys-delay 0.2))
  (use-package use-package-chords :config (key-chord-mode 1))
  (use-package server :config (unless (server-running-p) (server-start)))
  (use-package restart-emacs)
  (use-package session)
  (use-package hydra)
  (use-package which-key
    :config
    (progn
      (which-key-mode)
      (which-key-setup-side-window-bottom)
      ;; (which-key-setup-minibuffer)
      ))

  (use-package ivy :demand t
    :diminish (ivy-mode . "")
    :bind     ("C-c C-r" . ivy-resume)
    :config   (ivy-mode 1))

  (use-package swiper :bind ("C-s" . swiper))

  (use-package counsel
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
    :diminish (projectile-mode . " ⓟ")
    :commands (projectile-global-mode
               projectile-mode
               hydra-projectile/body
               hydra-projectile-if-projectile-p
               projectile-project-root
               projectile-project-p)
    :bind (("s-p" . hydra-projectile/body)
           ("s-b" . counsel-projectile-switch-to-buffer))
    :config
    (progn 
      (projectile-global-mode 1)

      ;; https://github.com/ericdanan/counsel-projectile
      (use-package counsel-projectile :config (counsel-projectile-on))

      (defhydra hydra-projectile
        (:color teal :hint nil :pre (projectile-mode))
        "
     PROJECTILE: %(projectile-project-root)
    ^FIND FILE^        ^SEARCH/TAGS^        ^BUFFERS^       ^CACHE^                    ^PROJECT^
    _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
    _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd      _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
    _r_: recent file   ^ ^                  ^ ^             _z_: cache current
    _d_: dir
"
        ("a"   counsel-projectile-ag)
        ("b"   counsel-projectile-switch-to-buffer)
        ("c"   projectile-invalidate-cache)
        ("d"   counsel-projectile-find-dir)
        ("f"   counsel-projectile-find-file)
        ("F"   projectile-find-file-dwim)
        ("C-f" projectile-find-file-in-directory)
        ("g"   ggtags-update-tags)
        ("s-g" ggtags-update-tags)
        ("i"   projectile-ibuffer)
        ("K"   projectile-kill-buffers)
        ("s-k" projectile-kill-buffers)
        ("m"   projectile-multi-occur)
        ("o"   projectile-multi-occur)
        ("p"   counsel-projectile-switch-project)
        ("r"   projectile-recentf)
        ("x"   projectile-remove-known-project)
        ("X"   projectile-cleanup-known-projects)
        ("z"   projectile-cache-current-file)
        ("q"   nil "cancel" :color blue))

      (defun hydra-projectile-if-projectile-p ()
        (interactive)
        (if (projectile-project-p)
            (hydra-projectile/body)
          (counsel-projectile)))))

  (use-package exec-path-from-shell
    :defer 2
    :commands (exec-path-from-shell-initialize
               exec-path-from-shell-copy-env)
    :config
    (exec-path-from-shell-initialize))

  ) ;; let ends here

;; ------ global keybindings
;; C-
(general-define-key "C-a" 'my-beginning-of-line
                    "C-M-j" 'delete-indentation-forward
                    "C-<tab>" 'switch-to-previous-buffer
                    "C-<return>" 'my-other-window
                    "C-. m" 'kmacro-keymap
                    "C-. C-i" 'indent-rigidly)

;; M-
(general-define-key "M-!" 'async-shell-command
                    "M-'" 'insert-pair
                    "<M-backspace>" 'contextual-backspace
                    "M-[" 'align-code
                    "M-`" 'other-frame
                    "M-W" 'mark-word
                    "M-L" 'mark-line
                    "M-S" 'mark-sentence
                    "M-X" 'mark-sexp
                    "M-D" 'mark-defun
                    "M-g c" 'goto-char
                    "M-g l" 'goto-line)

;; C-x-
(general-define-key :prefix "C-x"
                    "d" 'delete-whitespace-rectangle
                    "F" 'set-fill-column
                    "t" 'toggle-truncate-lines
                    "_" 'split-window-below
                    "|" 'split-window-right
                    "K" 'delete-current-buffer-file
                    "y" '(lambda() (interactive) (insert-separator nil))
                    "Y" '(lambda() (interactive) (insert-separator t))
                    "v" 'show-magic-info
                    ;; ------ C-x C-
                    "C-d" 'duplicate-line
                    "C-e" 'pp-eval-last-sexp
                    "C-n" 'next-line
                    "C-o" 'kill-other-buffers
                    "C-v" 'find-alternate-file-with-sudo
                    "M-n" 'set-goal-column
                    "M-q" 'refill-paragraph)

;; C-c-
(general-define-key :prefix "C-c"
                    "SPC" 'just-one-space
                    "C-f" 'my-code-format
                    "0" (recursive-edit-preserving-window-config
                         (delete-window))
                    "1" (recursive-edit-preserving-window-config
                         (if (one-window-p 'ignore-minibuffer)
                             (error "Current window is the only window in its frame")
                           (delete-other-windows)))
                    "f" 'flush-lines
                    "g" 'goto-line
                    "k" 'keep-lines
                    "o" 'customize-option
                    "O" 'customize-group
                    "F" 'customize-face
                    "q" 'fill-region
                    "r" 'replace-regexp
                    "s" 'replace-string
                    "u" 'rename-uniquely
                    "v" 'ffap
                    "V" 'view-clipboard
                    "z" 'clean-buffer-list
                    "[" 'align-regexp
                    "=" 'count-matches
                    ";" 'comment-or-uncomment-region
                    "C-z" 'delete-to-end-of-buffer
                    "M-q" 'unfill-paragraph)

(general-define-key :prefix "C-c e"
                    "E" 'elint-current-buffer
                    "b" 'do-eval-buffer
                    "c" 'cancel-debug-on-entry
                    "d" 'debug-on-entry
                    "e" 'toggle-debug-on-error
                    "f" 'emacs-lisp-byte-compile-and-load
                    "j" 'emacs-lisp-mode
                    "l" 'find-library
                    "r" 'do-eval-region
                    "s" 'scratch
                    "z" 'byte-recompile-directory)
;; C-h e -
(defvar blaise--lisp-find-map)
(define-prefix-command 'blaine--lisp-find-map)
(bind-key "C-h e" 'blaine--lisp-find-map)
(general-define-key :keymaps 'blaine--lisp-find-map
                    "c" 'finder-commentary
                    "e" 'view-echo-area-messages
                    "f" 'find-function
                    "F" 'find-face-definition
                    "d" 'my-describe-symbol
                    "i" 'info-apropos
                    "k" 'find-function-on-key
                    "l" 'find-library
                    "s" 'scratch
                    "v" 'find-variable
                    "V" 'apropos-value
                    "t" 'what-face)
