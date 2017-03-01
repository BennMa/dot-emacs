(general-define-key "M-."                'counsel-gtags-dwim
                    "M-,"                'counsel-gtags-pop
                    "C-<tab>"            'blaine/last-buffer
                    "C-y"                'ivy-switch-buffer
                    "C-S-y"              'counsel-projectile-switch-to-buffer
                    "C-x C-r"            'counsel-recentf
                    ;; "C-y"             'hydra-buffer/body
                    "C-w"                'hydra-window/body
                    "C-M-w"              'blaine/other-window
                    "C-x 0"              'sticky-window-delete-window
                    "C-x 1"              'sticky-window-delete-other-windows
                    (general-chord "jj") 'avy-goto-char
                    (general-chord "jw") 'avy-goto-word-1
                    (general-chord "jl") 'avy-goto-line)

(defhydra hydra-buffer (:color blue :columns 4 :exit t)
  "Buffers Switcher"
  ("b"   blaine/last-buffer "Last Buffer")
  ("l"   ivy-switch-buffer "All Buffers")
  ("C-l"   ivy-switch-buffer "All Buffers")
  ("p"   counsel-projectile-switch-to-buffer "Project Buffers")
  ("s"   save-buffer "Save" :color red)
  ("k"   kill-this-buffer "Kill this buffer"  :color red)
  ("d"   blaine/delete-current-buffer-file "Delete This Buffer" :color red)
  ("B"   blaine/ibuffer-startup "Ibuffer")
  ("M-b" buffer-menu "Buffer Menu")
  ("."   hydra-window/body "Windows")
  ("q"   nil "Cancel" :color blue))

(defhydra hydra-window (:hint nil :color amaranth :columns 3 :exit t :idle 0.3)
   "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^ ^^^^   ^COMMAND^   ^WINDOW^
^ ^ _k_ ^ ^   _-_ : split H    ^ ^ _p_ ^ ^   _d_elete    ^1^ ^2^ ^3^ ^4^
_h_ _a_ _l_   _|_ : split V    _b_ ^=^ _f_   _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _j_ ^ ^   _s_ : split H    ^ ^ _n_ ^ ^   _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^ ^ ^ ^ ^   _D_edicated
"
  ("h" windmove-left :color blue)
  ("l" windmove-right :color blue)
  ("j" windmove-down :color blue )
  ("k" windmove-up :color blue)

  ;; size
  ("p" (lambda () (interactive) (enlarge-window -1)))
  ("b" enlarge-window-horizontally)
  ("f" (lambda () (interactive) (enlarge-window-horizontally -1)))
  ("n" (lambda () (interactive) (enlarge-window 1)))

  ;; splt
  ("-" blaine/split-window-below-and-focus)
  ("s" blaine/split-window-below-and-focus)
  ("|" blaine/split-window-right-and-focus)
  ("v" blaine/split-window-right-and-focus)

  ("u" winner-undo)
  ("m" sticky-window-delete-other-windows)
  ("d" sticky-window-delete-window)

  ("a" ace-window)
  ("=" balance-windows)
  ("D" dedicated-mode)

  ;; change height and width
  ("0" select-window-0 :color blue)
  ("1" select-window-1 :color blue)
  ("2" select-window-2 :color blue)
  ("3" select-window-3 :color blue)
  ("4" select-window-4 :color blue)
  ("5" select-window-5 :color blue)
  ("6" select-window-6 :color blue)
  ("7" select-window-7 :color blue)
  ("8" select-window-8 :color blue)
  ("9" select-window-9 :color blue)

  ;; ("D" kill-buffer-and-window "Delete Buffer" :color red)
  ("w" blaine/other-window "Other Window" :color blue)
  ("C-w" blaine/other-window "Other Window" :color blue)
  ("." hydra-buffer/body "Buffers" :color blue)
  ("q" nil "quit" :color blue))


;; ------ Packages
(progn ;; own functions
  (defcustom blaine--buffername-whitelist '()
    "white list of last buffer switcher"
    :type '(repeat regexp))
  (defcustom blaine--buffermode-whitelist '()
    "white list of modes of last buffer switcher"
    :type '(repeat string))
  (defcustom blaine--buffername-blacklist '()
    "black list of last buffer switcher"
    :type '(repeat regexp))
  (defcustom blaine--buffermode-blacklist '()
    "black list of modes of last buffer switcher"
    :type '(repeat string))
  (defun blaine/last-buffer ()
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
                                        blaine--buffername-whitelist)
                    (member-ignore-case last-buffer-mode
                                        blaine--buffermode-whitelist)
                    (not (or (list-regex-match-p last-buffer-name
                                                 blaine--buffername-blacklist)
                             (member-ignore-case last-buffer-mode
                                                 blaine--buffermode-blacklist))))
            (switch-to-buffer last-buffer-name)
            (throw 'matched_ t))
          (setq i (1+ i))
          nil)
        )))
  (defun blaine/delete-current-buffer-file ()
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
  (defun blaine/other-window (count &optional all-frames)
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
  (defun blaine/split-window-right-and-focus()
    (interactive)
    (call-interactively 'split-window-right)
    (call-interactively 'windmove-right))
  (defun blaine/split-window-below-and-focus()
    (interactive)
    (call-interactively 'split-window-below)
    (call-interactively 'windmove-down)))

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
    (defun counsel-gtags-create-or-update-tags ()
      (interactive)
      (if (or (getenv "GTAGSROOT")
              (locate-dominating-file default-directory "GTAGS"))
          (let ((current-prefix-arg 4))
            (call-interactively 'counsel-gtags-update-tags))
        (if (fboundp 'projectile-project-root)
            (counsel-gtags-create-tags (projectile-project-root) "default")
          (counsel-gtags--generate-tags))))))

(use-package ibuffer
  :commands (blaine/ibuffer-startup
             ibuffer)
  :config
  (progn
    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (ibuffer-auto-mode 1)
                  (ibuffer-switch-to-saved-filter-groups "default")))

    (defun blaine/ibuffer-startup ()
      "Open ibuffer with cursour pointed to most recent buffer name"
      (interactive)
      (let ((recent-buffer-name (buffer-name)))
        (ibuffer)
        (ibuffer-jump-to-buffer recent-buffer-name)))

    (defun blaine/ibuffer-never-show-predicates (buffer)
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
  :init (add-hook 'after-init-hook 'window-numbering-mode))

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
