;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-o <letter>  tmux
;;   M-p <letter>  projectile + personal map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;; ====== global-map

;; (define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))


;; ====== C-
;; (defvar ctl-oh-map)
;; (define-prefix-command 'ctl-oh-map)
;; (bind-key "C-o" 'ctl-oh-map)

;; (bind-key* "<C-return>" 'other-window)

;; (bind-key "C-z" 'delete-other-windows)

;; deprecated
(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)


;; ------ C-M-
(bind-key "<C-M-backspace>" 'backward-kill-sexp)
(bind-key "C-M-w" 'copy-line)
(bind-key "C-M-v" 'scroll-down-command)



;; ====== M-
;; (unbind-key "C-w")
;; (bind-key "M-x" 'kill-region)

;; rebind copy, paste, cut
(unbind-key "M-w")
(unbind-key "C-y")
(bind-key "M-c" 'kill-ring-save)
(bind-key "M-v" 'yank)
(bind-key "M-v" 'isearch-yank-kill isearch-mode-map)

(bind-key "M-!" 'async-shell-command)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(bind-key "M-L" 'mark-line)

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)



;; ====== C-x -
(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

(bind-key "C-x K" 'delete-current-buffer-file)

(bind-key "C-x y" '(lambda() (interactive) (insert-separator nil)))
(bind-key "C-x Y" '(lambda() (interactive) (insert-separator t)))

(bind-key* "C-x v" 'show-magic-info)

;; ------ C-x C-
(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;; ------ C-x M-
(bind-key "C-x M-n" 'set-goal-column)
(bind-key "C-x M-q" 'refill-paragraph)



;; ====== mode-specific-map, C-c

(bind-key "C-<tab>" 'switch-to-previous-buffer)

(bind-key [?\C-c ?\t] 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

(bind-key "C-c 0"
          (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
          (recursive-edit-preserving-window-config
           (if (one-window-p 'ignore-minibuffer)
               (error "Current window is the only window in its frame")
             (delete-other-windows))))

(bind-key "C-c d" 'delete-current-line)

(bind-keys :prefix-map my-lisp-devel-map
           :prefix "C-c e"
           ("E" . elint-current-buffer)
           ("b" . do-eval-buffer)
           ("c" . cancel-debug-on-entry)
           ("d" . debug-on-entry)
           ("e" . toggle-debug-on-error)
           ("f" . emacs-lisp-byte-compile-and-load)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))

(bind-key "C-c f" 'flush-lines)

(bind-key "C-c g" 'goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(bind-key "C-c k" 'keep-lines)

(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)
(bind-key "C-c F" 'customize-face)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(bind-key "C-c v" 'ffap)



(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;; ------ C-c C-

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;; ------ C-c M-

(bind-key "C-c M-q" 'unfill-paragraph)

;; ------ ctl-period-map, C-.

(bind-key "C-. m" 'kmacro-keymap)
(bind-key "C-. C-i" 'indent-rigidly)

;; ====== help-map

(defvar lisp-find-map)
(define-prefix-command 'lisp-find-map)

(bind-key "C-h e" 'lisp-find-map)

;; ------ C-h e

(bind-key "C-h e c" 'finder-commentary)
(bind-key "C-h e e" 'view-echo-area-messages)
(bind-key "C-h e f" 'find-function)
(bind-key "C-h e F" 'find-face-definition)


(bind-key "C-h e d" 'my-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)
(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)
(bind-key "C-h e t" 'what-face)
