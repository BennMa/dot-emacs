;; ------ global keybindings
(use-package hydra)
(use-package key-chord :defer 1 :config (setq key-chord-two-keys-delay 0.2))
(use-package diminish)
(use-package bind-key)
(use-package use-package-chords :config (key-chord-mode 1))
(use-package restart-emacs :commands restart-emacs)
(use-package server :config (unless (server-running-p) (server-start)))
(use-package general
  :config
  (progn
    (general-define-key "RET" 'newline-and-indent
                        "C-a" 'mwim-beginning-of-code-or-line
                        "C-e" 'mwim-end-of-code-or-line
                        "C-/" 'undo-tree-undo
                        "C-;" 'hydra-projectile-if-projectile-p
                        "C-x C-v" 'ivy-resume
                        "C-S-v" 'scroll-other-window
                        "C-M-v" 'scroll-down-command
                        "C-M-d" 'blaine/duplicate-line
                        "C-M-S-v" 'scroll-other-window-down
                        "M-!" 'async-shell-command
                        "M-`" 'other-frame
                        "M-y" 'counsel-yank-pop
                        "M-q" 'save-buffers-kill-terminal
                        "M-v" 'yank
                        "M-c" 'kill-ring-save
                        "M-x" '(lambda(m-x-p) (interactive "P")
                                 (if (and (use-region-p) (not m-x-p))
                                     (call-interactively 'kill-region)
                                   (call-interactively 'counsel-M-x)))
                        "M-w" '(lambda () (interactive)
                                 (kill-buffer (current-buffer)))
                        "M-w" 'kill-buffer-and-window ;;delete-window
                        ;; "M-n" 'make-frame
                        ;; "C-M-w" 'delete-frame
                        "M-z" 'undo-tree-undo
                        "M-r" 'undo-tree-redo
                        "M-s" '(lambda () (interactive)
                                 (call-interactively (key-binding "\C-x\C-s")))
                        "M-i" 'counsel-imenu)
    ;; C-x
    (general-define-key :prefix "C-x"
                        "d" 'delete-whitespace-rectangle
                        "F" 'set-fill-column
                        "t" 'toggle-truncate-lines
                        "y" '(lambda() (interactive) (blaine/insert-separator nil))
                        "Y" '(lambda() (interactive) (blaine/insert-separator t))
                        "v" 'blaine/buffer-info
                        "d" 'dired-jump
                        "C-e" 'pp-eval-last-sexp
                        "C-o" 'blaine/kill-other-buffers
                        "C-f" 'counsel-find-file)
    ;; C-c
    (general-define-key :prefix "C-c"
                        "SPC" 'just-one-space
                        "C-f" 'blaine/format-buffer
                        "f"   'flush-lines
                        "g"   'goto-line
                        "k"   'keep-lines
                        "o"   'customize-option
                        "O"   'customize-group
                        "F"   'customize-face
                        "l"   'counsel-find-library
                        "q"   'fill-region
                        "["   'align-regexp
                        "="   'count-matches
                        ";"   'comment-or-uncomment-region)
    ;; C-h
    (general-define-key "C-h e"   nil
                        "C-?"     '(lambda () (interactive)
                                     (if (eq major-mode 'makey-key-mode)
                                         (makey-key-mode-command nil)
                                       (call-interactively 'discover-my-major)))
                        "C-M-?"   'discover-my-mode
                        ;; "C-h C-m" 'describe-mode
                        )

    (general-define-key :prefix "C-h e"
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
    ;; C-c e
    (general-define-key "C-c e" (defhydra hydra-elisp-helper (:hint nil :color pink :columns 4 :exit t)
                                  "Elisp Helper"
                                  ("E" elint-current-buffer "Elint Buffer")
                                  ("b" (lambda () (interactive)
                                         (call-interactively 'eval-buffer)) "Eval Buffer")
                                  ("r" (lambda () (interactive)
                                         (call-interactively 'eval-region)) "Eval Region")
                                  ("C" cancel-debug-on-entry "Cancel Debug On Entry")
                                  ("d" debug-on-entry "Debug On Entry")
                                  ("e" toggle-debug-on-error "Toggle Debug On Error")
                                  ("f" emacs-lisp-byte-compile-and-load "Bytecompile And Load") 
                                  ("z" byte-recompile-directory "Byterecompile Dir")
                                  ("j" emacs-lisp-mode "Emacs Lisp Mode")
                                  ("q" nil "Cancel")))
    ))

;; ------ packages
(use-package session)

;; (use-package cus-edit)
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
    (general-define-key :keymaps 'ivy-minibuffer-map
                        "C-?"   (defhydra hydra-ivy (:hint nil :color pink :columns 4)
                                  "Ivy Helper"
                                  ("M-o" ivy-dispatching-done "Dispatching Done")
                                  ("C-j" ivy-alt-done "Alt Done")
                                  ("C-M-j" ivy-immediate-done "Immediate Done")
                                  ("C-'" ivy-avy "Avy")
                                  ("C-M-m" ivy-call "Call")
                                  ("C-M-n" ivy-next-line-and-call "Next line and Call")
                                  ("C-M-p" ivy-previous-line-and-call "Prev line and Call")
                                  ("C-M-o" ivy-dispatching-call "Dispatching Call")
                                  ("M-i" ivy-insert-current "Insert Current")
                                  ("M-j" ivy-yank-word "Yank Word")
                                  ("S-SPC" ivy-restrict-to-matches "Restricted Matches")
                                  ("C-r" ivy-reverse-i-search "Search History")
                                  ("M-w" ivy-kill-ring-save "Copy")
                                  ("C-c C-o" ivy-occur "Occur")
                                  ("C-?" nil "Cancel"))
                        "M-v"   'yank
                        "C-M-v" 'ivy-scroll-down-command)))

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
             counsel-unicode-char
             counsel-recentf
             counsel-yank-pop))

(use-package projectile
  :diminish (projectile-mode . " ⓟ")
  :commands (projectile-mode
             projectile-global-mode
             hydra-projectile/body
             hydra-projectile-if-projectile-p
             projectile-project-root
             projectile-project-p
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
    _f_: file          _a_: ag              _i_: Ibuffer    _k_: cache clear           _p_: switch proj
    _F_: file dwim     _g_: regenerate tag  _b_: switch to  _x_: remove known project
  _C-f_: file pwd      _c_: find definition _s-k_: Kill all _X_: cleanup non-existing
    _r_: recent file   _u_: find reference  ^ ^             _z_: cache current
    _d_: dir           _s_: find symbol
    ^ ^                _o_: multi-occur
    ^ ^                _,_: pop
"
      ("a"   counsel-projectile-ag)
      ("b"   counsel-projectile-switch-to-buffer)
      ("k"   projectile-invalidate-cache)
      ("d"   counsel-projectile-find-dir)
      ("f"   counsel-projectile-find-file)
      ("F"   projectile-find-file-dwim)
      ("C-f" projectile-find-file-in-directory)
      ;; ("g"   projectile-regenerate-tags) ;; regenerate tags by ggtags
      ("g"   counsel-gtags-create-or-update-tags)
      ("c"   counsel-gtags-find-definition)
      ("."   counsel-gtags-find-definition)
      (","   counsel-gtags-pop)
      ("u"   counsel-gtags-find-reference)
      ("s"   counsel-gtags-find-symbol)
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
  :config
  (progn
    (global-undo-tree-mode)
    (general-define-key :keymaps 'undo-tree-map "C-?" nil)))

(use-package exec-path-from-shell
  :defer 2
  :if (memq window-system '(mac ns))
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :config (exec-path-from-shell-initialize))

(use-package discover-my-major
  :commands (discover-my-major discover-my-mode))

(use-package mwim
  :commands (mwim
             mwim-beginning
             mwim-end
             mwim-beginning-of-code-or-line
             mwim-beginning-of-line-or-code
             mwim-beginning-of-code-or-line-or-comment
             mwim-end-of-code-or-line
             mwim-end-of-line-or-code))
