(use-package scala-mode
  :mode ("\\.scala\\'"  . scala-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (setq comment-start "/* "
                    comment-end " */"
                    comment-style 'multi-line
                    comment-empty-lines t)
              ;; (setq prettify-symbols-alist scala-prettify-symbols-alist)
              ;; (prettify-symbols-mode)
              ;; (ensime-mode 1)
              ))

  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (call-interactively 'newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (add-hook 'sbt-mode-hook 
            (lambda ()
              (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers))))

(use-package ensime
  :pin melpa-stable
  :disabled t
  :commands (ensime-mode)
  :init
  (progn
    (setq ensime-startup-snapshot-notification nil)))
