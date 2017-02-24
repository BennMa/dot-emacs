;; ------ global keybindings
(use-package general
  :config
  (progn
    (use-package key-chord :defer 1 :config (setq key-chord-two-keys-delay 0.2))
    (general-define-key "RET" 'newline-and-indent)
    ;; C-
    (general-define-key "C-a" 'blaine/beginning-of-line
                        "C-M-j" '(lambda() (interactive) (delete-indentation t))
                        "C-c C-r" 'ivy-resume
                        "C-;" 'hydra-projectile/body)
    ;; C-x-
    (general-define-key :prefix "C-x"
                        "d" 'delete-whitespace-rectangle
                        "F" 'set-fill-column
                        "t" 'toggle-truncate-lines
                        "y" '(lambda() (interactive) (blaine/insert-separator nil))
                        "Y" '(lambda() (interactive) (blaine/insert-separator t))
                        "v" 'blaine/buffer-info
                        ;; ------ C-x C-
                        "C-d" 'blaine/duplicate-line
                        "C-j" 'dired-jump
                        "C-e" 'pp-eval-last-sexp
                        "C-o" 'blaine/kill-other-buffers
                        "C-f" 'counsel-find-file)
    ;; C-c-
    (general-define-key :prefix "C-c"
                        "SPC" 'just-one-space
                        "C-f" 'blaine/format-buffer
                        "f"   'flush-lines
                        "g"   'goto-line
                        "k"   'keep-lines
                        "o"   'customize-option
                        "O"   'customize-group
                        "F"   'customize-face
                        "q"   'fill-region
                        "["   'align-regexp
                        "="   'count-matches
                        ";"   'comment-or-uncomment-region)
    ;; M-
    (general-define-key "M-!" 'async-shell-command
                        "<M-backspace>" 'blaine/contextual-backspace
                        "M-`" 'other-frame
                        "M-." 'projectile-find-tag
                        "M-," 'pop-tag-mark
                        "M-Y" 'counsel-yank-pop)
    ;; system related, like copy&paste
    (general-define-key "M-q" 'save-buffers-kill-terminal
                        "M-v" 'yank
                        "M-c" 'kill-ring-save
                        "M-x" '(lambda() (interactive)
                                 (if (use-region-p)
                                     (call-interactively 'kill-region)
                                   (call-interactively 'counsel-M-x)))
                        "M-w" '(lambda () (interactive) (kill-buffer (current-buffer)))
                        "C-M-w" 'kill-buffer-and-window ;;delete-window
                        "M-W" 'delete-frame
                        ;; "M-n" 'make-frame
                        "C-/" 'undo-tree-undo
                        "M-z" 'undo-tree-undo
                        "M-r" 'undo-tree-redo
                        ;; "M-Z" 'undo-tree-undo
                        "M-s" (lambda () (interactive)
                                (call-interactively (key-binding "\C-x\C-s")))
                        "M-i" 'counsel-imenu
                        "C-M-v" 'scroll-down-command)
    (with-eval-after-load 'term
      (general-define-key :keymaps 'term-raw-map "M-v" 'term-paste))
    ;; C-c e -
    (general-define-key :prefix "C-c e"
                        "E" 'elint-current-buffer
                        "b" '(lambda () (interactive) (call-interactively 'eval-buffer))
                        "C" 'cancel-debug-on-entry
                        "d" 'debug-on-entry
                        "e" 'toggle-debug-on-error
                        "f" 'emacs-lisp-byte-compile-and-load
                        "j" 'emacs-lisp-mode
                        "l" 'find-library
                        "r" '(lambda () (interactive) (call-interactively 'eval-region))
                        "z" 'byte-recompile-directory)
    ;; C-h e -
    (define-prefix-command 'blaine--lisp-find-map)
    (bind-key "C-h e" 'blaine--lisp-find-map)
    (general-define-key :keymaps 'blaine--lisp-find-map
                        "c" 'finder-commentary
                        "e" 'view-echo-area-messages
                        "f" 'find-function
                        "F" 'find-face-definition
                        "i" 'info-apropos
                        "k" 'find-function-on-key
                        "l" 'find-library
                        "s" 'blaine/scratch
                        "v" 'find-variable
                        "V" 'apropos-value
                        "t" 'blaine/what-face)
    ))

;; ------ packages
(let ((use-package-always-ensure t))

  (use-package diminish)
  (use-package bind-key)
  (use-package use-package-chords :config (key-chord-mode 1))
  (use-package server :config (unless (server-running-p) (server-start)))
  (use-package restart-emacs :commands restart-emacs)
  (use-package session)
  ;; (use-package cus-edit)
  (use-package hydra)
  (use-package which-key
    :diminish (which-key-mode . " ⓦ")
    :config
    (progn
      (which-key-mode)
      (which-key-setup-side-window-bottom)
      ;; (which-key-setup-minibuffer)
      ))

  (use-package ivy :demand t
    :diminish (ivy-mode . "")
    :config
    (progn
      (ivy-mode 1)
      (define-key ivy-minibuffer-map "\C-o"
        (defhydra hydra-ivy (:hint nil :color pink :columns 4)
          "Ivy Helper"
          ("C-o" nil)
          ("M-o" ivy-dispatching-done "Dispatching Done")
          ("C-j" ivy-alt-done "Alt Done")
          ("C-M-j" ivy-immediate-done "Immediate Done")
          ("C-'" ivy-avy "Avy")
          ("C-M-m" ivy-call "Call")
          ("C-M-o" ivy-dispatching-call "Dispatching Call")
          ("M-i" ivy-insert-current "Insert Current")
          ("M-j" ivy-yank-word "Yank Word")
          ("S-SPC" ivy-restrict-to-matches "Restricted Matches")
          ("C-r" ivy-reverse-i-search "Search History")
          ("M-w" ivy-kill-ring-save "Copy")))))

  (use-package counsel
    :commands (counsel-M-x
               counsel-find-file
               counsel-git
               counsel-git-grep
               counsel-ag
               counsel-locate
               counsel-imenu
               counsel-describe-function
               counsel-describe-variable
               counsel-find-library
               counsel-info-lookup-symbol
               counsel-unicode-char))

  (use-package projectile
    :diminish (projectile-mode . " ⓟ")
    :commands (projectile-mode
               projectile-global-mode
               hydra-projectile/body
               hydra-projectile-if-projectile-p
               projectile-project-root
               projectile-project-p
               projectile-find-tag
               counsel-projectile-switch-to-buffer)    
    :config
    (progn 
      (projectile-global-mode 1)
      ;; https://github.com/ericdanan/counsel-projectile
      (use-package counsel-projectile :config (counsel-projectile-on))
      (defun hydra-projectile-if-projectile-p ()
        (interactive)
        (if (projectile-project-p)
            (hydra-projectile/body)
          (counsel-projectile)))

      (defhydra hydra-projectile (:color teal :hint nil :idle 0.3)
        "
     PROJECTILE: %(projectile-project-root)
    ^FIND FILE^        ^SEARCH/TAGS^        ^BUFFERS^       ^CACHE^                    ^PROJECT^
    _f_: file          _s_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
    _F_: file dwim     _g_: update tag      _b_: switch to  _x_: remove known project
  _C-f_: file pwd      _._: find tag        _s-k_: Kill all   _X_: cleanup non-existing
    _r_: recent file   _o_: multi-occur     ^ ^             _z_: cache current
    _d_: dir
"
        ("s"   counsel-projectile-ag)
        ("b"   counsel-projectile-switch-to-buffer)
        ("c"   projectile-invalidate-cache)
        ("d"   counsel-projectile-find-dir)
        ("f"   counsel-projectile-find-file)
        ("F"   projectile-find-file-dwim)
        ("C-f" projectile-find-file-in-directory)
        ("g"   projectile-regenerate-tags)
        ("."   projectile-find-tag)
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
        ("q"   nil "cancel" :color blue))))

  (use-package undo-tree
    :commands (undo-tree-visualize
               undo-tree-undo
               undo-tree-redo)
    :config (global-undo-tree-mode))

  (use-package exec-path-from-shell
    :defer 2
    :commands (exec-path-from-shell-initialize
               exec-path-from-shell-copy-env)
    :config
    (exec-path-from-shell-initialize))

  ) ;; let ends here
