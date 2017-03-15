(general-define-key "C-c m" 'hydra-misc/body)
(defhydra hydra-misc (:hint nil :color blue :columns 4)
  "Misc Helper"
  ("m" emacs-toggle-size "Toggle frame size")
  ("M" emacs-max "Max frame")
  ("e" escreen-create-screen "Create new screen")
  ("z" hydra-zoom/body "Zoom")
  ("g" google-this "Google this")
  ("s" sql-pool "SQL Pool")
  ("r" regex-tool "Regex tool")
  ("q" nil "Cancel"))

;; ------ packages
(use-package maxframe
  :commands (emacs-max
             emacs-toggle-size)
  :config
  (progn
    (defvar emacs-min-top 23)
    (defvar emacs-min-left 0)
    (defvar emacs-min-width 85)
    (defvar emacs-min-height 35)
    (defun emacs-min ()
      (interactive)
      (set-frame-parameter (selected-frame) 'fullscreen nil)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'top emacs-min-top)
      (set-frame-parameter (selected-frame) 'left emacs-min-left)
      (set-frame-parameter (selected-frame) 'height emacs-min-height)
      (set-frame-parameter (selected-frame) 'width emacs-min-width))
    (defun emacs-max ()
      (interactive)
      (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
      (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
      (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))
    (defun emacs-toggle-size ()
      (interactive)
      (if (> (cdr (assq 'width (frame-parameters))) 100)
          (emacs-min)
        (maximize-frame)))))

(use-package escreen
  :commands (escreen-create-screen)
  :bind (:map escreen-map
              ("e" . escreen-goto-last-screen)
              ("m" . escreen-menu))
  :config (escreen-install))

(use-package zoom-frm
  :commands (zoom-frm-in
             zoom-frm-out
             zoom-frm-unzoom
             zoom-in
             zoom-out
             hydra-zoom/body)
  :config
  (setq zoom-frame/buffer 'buffer)

  (defhydra hydra-zoom (:hint nil)
    "
  ^BUFFER^   ^FRAME^    ^ACTION^
  _t_: +     _T_: +     _0_: reset
  _s_: -     _S_: -     _q_: quit
"
    ("t" zoom-in )
    ("s" zoom-out )
    ("T" zoom-frm-in )
    ("S" zoom-frm-out )
    ("0" zoom-frm-unzoom)
    ("q" nil :color blue)))

(use-package powerline
  :disabled t
  :config (powerline-default-theme))
(use-package smart-mode-line
  ;; :defer 2
  :config (sml/setup))

(use-package paradox :commands paradox-list-packages)

(use-package google-this
  :commands (google-this)
  ;; :init (setq google-this-keybind (kbd "C-c m g"))
  ;; :config (google-this-mode 1)
  )

;; https://github.com/kiwanami/emacs-edbi
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
(use-package sql
  :commands (sql-pool)
  :config
  (progn
    (defun sql-pool ()
      (interactive)
      (ivy-read "SQL Pool: " sql-connection-alist
                :action (lambda (connection)
                          (my/sql-connect-preset (car connection)))))

    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (setq sql-alternate-buffer-name (my/sql-make-smart-buffer-name))
                (sql-rename-buffer)))

    (defun my/sql-connect-preset (name)
      "Connect to a predefined SQL connection listed in `sql-connection-alist'"
      (eval `(let ,(cdr (assoc name sql-connection-alist))
               (flet ((sql-get-login (&rest what)))
                 (sql-product-interactive sql-product)))))

    ;; names the buffer *SQL: <host>_<db>, which is easier to 
    ;; find when you M-x list-buffers, or C-x C-b
    (defun my/sql-make-smart-buffer-name ()
      "Return a string that can be used to rename a SQLi buffer.
  This is used to set `sql-alternate-buffer-name' within
  `sql-interactive-mode'."
      (or (and (boundp 'sql-name) sql-name)
          (concat (if (not(string= "" sql-server))
                      (concat
                       (or (and (string-match "[0-9.]+" sql-server) sql-server)
                           (car (split-string sql-server "\\.")))
                       "/"))
                  sql-database)))))

(use-package mode-icons
  :defer 8
  :config (mode-icons-mode))

(use-package regex-tool
  :commands (regex-tool))

;;; misc.el ends here
