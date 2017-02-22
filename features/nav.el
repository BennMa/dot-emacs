;; ------ Keybindings
(general-define-key "C-<tab>"            'blaine/last-buffer
                    "C-y"                'ivy-switch-buffer
                    "C-S-y"              'counsel-recentf
                    "C-M-y"              'counsel-projectile-switch-to-buffer
                    ;; "C-y"                'hydra-buffer/body
                    "C-w"                'hydra-window/body
                    (general-chord "jj") 'avy-goto-char
                    (general-chord "jw") 'avy-goto-word-1
                    (general-chord "jl") 'avy-goto-line)

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
    (let ((i 1))
      (catch 'matched_
        (while (< i 6)
          (other-window count all-frames)
          (when (not (member major-mode '(direx:direx-mode project-explorer-mode)))
            (throw 'matched_ t))
          (setq i (1+ i)))))))

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

(use-package window-numbering :ensure t
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
             select-window-9))

(use-package ace-window :ensure t
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

;; ------ Hydra
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

(defhydra hydra-window (:hint nil :color amaranth :columns 3 :pre (progn (winner-mode 1) (window-numbering-mode 1)) :exit t)
   "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^ ^^^^   ^COMMAND^   ^WINDOW^
^ ^ _k_ ^ ^   _-_ : split H    ^ ^ _p_ ^ ^   _d_elete    ^1^ ^2^ ^3^ ^4^
_h_ _a_ _l_   _|_ : split V    _b_ ^=^ _f_   _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _j_ ^ ^   _s_ : split H    ^ ^ _n_ ^ ^   _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^ ^ ^ ^ ^   _r_edo
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
  ("-" split-window-vertically)
  ("s" split-window-vertically)
  ("|" split-window-horizontally)
  ("v" split-window-horizontally)

  ("u" winner-undo)
  ("r" winner-redo)
  ("m" delete-other-windows)
  ("d" delete-window)
  
  ("a" ace-window)
  ("=" balance-windows)

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

  ("D" kill-buffer-and-window "Delete Buffer" :color red)
  ("w" blaine/other-window "Other Window" :color blue)
  ("C-w" blaine/other-window "Other Window" :color blue)
  ("." hydra-buffer/body "Buffers" :color blue)
  ("q" nil "quit" :color blue))
