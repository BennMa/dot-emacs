(general-define-key "M-."                'counsel-gtags-dwim
                    "M-,"                'counsel-gtags-pop
                    "C-<tab>"            'my/last-buffer
                    "C-y"                'ivy-switch-buffer
                    "C-S-y"              'counsel-projectile-switch-to-buffer
                    "C-x C-r"            'counsel-recentf
                    ;; "C-y"             'hydra-buffer/body
                    "C-o"                'hydra-window/body
                    "C-M-o"              'my/other-window
                    "C-x 0"              'sticky-window-delete-window
                    "C-x 1"              'sticky-window-delete-other-windows
                    "C-j"                'avy-goto-char
                    "C-S-j"              'avy-goto-line
                    ;; (general-chord "jj") 'avy-goto-char
                    ;; (general-chord "jw") 'avy-goto-word-1
                    ;; (general-chord "jl") 'avy-goto-line
                    )

(defhydra hydra-buffer (:color blue :columns 4 :exit t)
  "Buffers Switcher"
  ("b"   my/last-buffer "Last Buffer")
  ("l"   ivy-switch-buffer "All Buffers")
  ("C-l"   ivy-switch-buffer "All Buffers")
  ("p"   counsel-projectile-switch-to-buffer "Project Buffers")
  ("s"   save-buffer "Save" :color red)
  ("k"   kill-this-buffer "Kill this buffer"  :color red)
  ("d"   my/delete-current-buffer-file "Delete This Buffer" :color red)
  ("B"   my/ibuffer-startup "Ibuffer")
  ("M-b" buffer-menu "Buffer Menu")
  ("."   hydra-window/body "Windows")
  ("q"   nil "Cancel" :color blue))

(defhydra hydra-window (:hint nil :columns 3 :color teal :idle 0.5)
   "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^ ^^^^   ^COMMAND^   ^WINDOW^
^ ^ _k_ ^ ^   _-_ : split H    ^ ^ _p_ ^ ^   _d_elete    ^1^ ^2^ ^3^ ^4^
_h_ _a_ _l_   _|_ : split V    _b_ ^=^ _f_   _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _j_ ^ ^   _s_ : split H    ^ ^ _n_ ^ ^   _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^ ^ ^ ^ ^   _D_edicated
^ ^ ^ ^ ^ ^   ^ ^ ^ ^ ^ ^ ^    ^ ^ ^ ^ ^ ^   _t_ranspose
"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down )
  ("k" windmove-up)

  ;; size
  ("p" (lambda () (interactive) (enlarge-window 1)) :color red)
  ("n" (lambda () (interactive) (enlarge-window -1)) :color red)
  ("b" (lambda () (interactive) (enlarge-window-horizontally -1)) :color red)
  ("f" enlarge-window-horizontally :color red)

  ;; splt
  ("-" my/split-window-below-and-focus)
  ("s" my/split-window-below-and-focus)
  ("|" my/split-window-right-and-focus)
  ("v" my/split-window-right-and-focus)

  ("u" winner-undo)
  ("m" sticky-window-delete-other-windows)
  ("d" sticky-window-delete-window)

  ("a" ace-window)
  ("=" balance-windows)
  ("D" dedicated-mode)
  ("t" my/transpose-windows)

  ;; change height and width
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)

  ;; ("D" kill-buffer-and-window "Delete Buffer" :color red)
  ("w" my/other-window "Other Window")
  ("C-w" my/other-window "Other Window")
  ("." hydra-buffer/body "Buffers")
  ("q" nil "quit"))


;; ------ Packages
(progn ;; own functions
  (defcustom my--buffername-whitelist '()
    "white list of last buffer switcher"
    :type '(repeat regexp))
  (defcustom my--buffermode-whitelist '()
    "white list of modes of last buffer switcher"
    :type '(repeat string))
  (defcustom my--buffername-blacklist '()
    "black list of last buffer switcher"
    :type '(repeat regexp))
  (defcustom my--buffermode-blacklist '()
    "black list of modes of last buffer switcher"
    :type '(repeat string))
  (defun my/last-buffer ()
    "switch to last active buffer by white list and black list"
    (interactive)
    (let ((i 1)
          last-buffer
          last-buffer-name
          last-buffer-mode)
      (catch 'matched_
        (while (< i 50)
          (setq last-buffer (nth i (buffer-list)))
          (setq last-buffer-name (buffer-name last-buffer))
          (setq last-buffer-mode (if last-buffer
                                     (symbol-name (buffer-local-value 'major-mode last-buffer))
                                   ""))
          (when (or (list-regex-match-p last-buffer-name
                                        my--buffername-whitelist)
                    (member-ignore-case last-buffer-mode
                                        my--buffermode-whitelist)
                    (not (or (list-regex-match-p last-buffer-name
                                                 my--buffername-blacklist)
                             (member-ignore-case last-buffer-mode
                                                 my--buffermode-blacklist))))
            (switch-to-buffer last-buffer-name)
            (throw 'matched_ t))
          (setq i (1+ i))
          nil)
        )))
  (defun my/delete-current-buffer-file ()
    "Delete the current buffer and the file connected with it"
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
      (if (not (and filename (file-exists-p filename)))
          (kill-buffer buffer)
        (when (yes-or-no-p "Are you sure, want to remove this file? ")
          (delete-file filename)
          (kill-buffer buffer)
          (message "File '%s' successfully removed" filename)))))
  (defun my/other-window (count &optional all-frames)
    (interactive "p")
    (let ((i 1)
          (black-mode-list '(;; direx:direx-mode
                             project-explorer-mode)))
      (catch 'matched_
        (while (< i 6)
          (other-window count all-frames)
          (when (not (member major-mode black-mode-list))
            (throw 'matched_ t))
          (setq i (1+ i))))))
  (defun my/split-window-right-and-focus()
    (interactive)
    (call-interactively 'split-window-right)
    (call-interactively 'windmove-right))
  (defun my/split-window-below-and-focus()
    (interactive)
    (call-interactively 'split-window-below)
    (call-interactively 'windmove-down))
  (defun my/transpose-windows (arg)
    "Transpose the buffers shown in two windows."
    (interactive "p")
    (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
      (while (/= arg 0)
        (let ((this-win (window-buffer))
              (next-win (window-buffer (funcall selector))))
          (set-window-buffer (selected-window) next-win)
          (set-window-buffer (funcall selector) this-win)
          (select-window (funcall selector)))
        (setq arg (if (plusp arg) (1- arg) (1+ arg)))))))

;; C-x C-<SPC>	go back in global-mark-ring, respects prefix arg
;; C-x C-<left>	go back in global-mark-ring
;; C-x C-<right>	go forward in global-mark-ring
;; C-x <SPC>	go back in (buffer-local) mark-ring, respects prefix arg
;; C-x <left>	go back in (buffer-local) mark-ring
;; C-x <right>	go forward in (buffer-local) mark-ring
(use-package back-button
  :diminish ""
  :defer 3
  :commands back-button-mode
  :config (back-button-mode))

(use-package ggtags :diminish "ⓖ"
  :commands (ggtags-mode
             ggtags-global-mode
             ggtags-find-tag-dwim
             ggtags-find-tag-mouse
             ggtags-find-definition
             ggtags-find-reference
             ggtags-find-other-symbol
             ggtags-find-tag-regexp
             ggtags-idutils-query
             ggtags-grep
             ggtags-find-file
             ggtags-query-replace)
  ;; :bind* (("M-." . ggtags-find-tag-dwim)
  ;;         ("M-," . pop-tag-mark))
  :config (ggtags-global-mode))

(use-package counsel-gtags
  :diminish ""
  :commands (counsel-gtags-dwim
             counsel-gtags-pop
             counsel-gtags-find-definition
             counsel-gtags-find-reference
             counsel-gtags-find-symbol
             counsel-gtags-find-file
             counsel-gtags-create-tags
             counsel-gtags-update-tags
             counsel-gtags-create-or-update-tags)
  :config
  (progn
    (defun counsel-gtags-create-or-update-tags (&optional no-create-p)
      (interactive)
      (if (or (getenv "GTAGSROOT")
              (locate-dominating-file default-directory "GTAGS"))
          (let ((current-prefix-arg 4))
            (call-interactively 'counsel-gtags-update-tags))
        (unless no-create-p
            (if (fboundp 'projectile-project-root)
                (counsel-gtags-create-tags (projectile-project-root) "default")
              (counsel-gtags--generate-tags)))))))

(use-package ibuffer
  :commands (my/ibuffer-startup
             ibuffer)
  :config
  (progn
    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (ibuffer-auto-mode 1)
                  (ibuffer-switch-to-saved-filter-groups "default")))

    (defun my/ibuffer-startup ()
      "Open ibuffer with cursour pointed to most recent buffer name"
      (interactive)
      (let ((recent-buffer-name (buffer-name)))
        (ibuffer)
        (ibuffer-jump-to-buffer recent-buffer-name)))

    (defun my/ibuffer-never-show-predicates (buffer)
      (let ((name (buffer-name buffer)))
        (and (not (or (member name '("*scratch*", "*Messages*"))
                      (string-match-p "^\\*terminal" name)))
             (or (string-match-p "^ ?\\*" name)
                 (string-match-p "^TAGS" name)))))

    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))))

(use-package window-numbering
  :defer 3
  :commands (window-numbering-mode
             select-window-0
             select-window-1
             select-window-2
             select-window-3
             select-window-4
             select-window-5
             select-window-6
             select-window-7
             select-window-8
             select-window-9)
  :config (window-numbering-mode))

(use-package ace-window
  :commands (ace-window)
  :config
  (progn
    (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
    (setq aw-background t)
    (setq aw-ignore-current t)))

(use-package ace-jump-mode
  :commands (ace-jump-char-mode
             ace-jump-word-mode
             ace-jump-line-mode))

(use-package avy
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-char-timer
             avy-goto-line
             avy-goto-word-1
             avy-goto-word-0))

(use-package winner
  ;; :if (not noninteractive)
  :commands (winner-redo winner-undo)
  :config (winner-mode 1))

(use-package recentf
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package sticky-windows :ensure nil
  :commands (sticky-window-delete-window
             sticky-window-delete-other-windows
             sticky-window-keep-window-visible))

(use-package dedicated
  :commands dedicated-mode)

;;; nav.el ends here
