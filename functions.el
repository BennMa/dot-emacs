;;; init.el
;;; Commentary:
;;

;;; Code:

;; ------ utility functions
(defun multiple-mode-add-hook (modes hook)
  "Given a list of x-mode-hook symbols in MODE, add the HOOK to them."
  (mapc (lambda (mode) (add-hook mode hook)) modes))

(defun list-regex-match-p (string list)
  (catch 'matched_ (dolist (regex list)
                     (if (string-match regex string)
                         (throw 'matched_ t))) nil))

(defun directory-files-recursively (DIRECTORY &optional FULL MATCH NOSORT MAX-DEPTH)
  (or MAX-DEPTH (setq MAX-DEPTH 5))
  (if (> MAX-DEPTH 1)
      (let ((temp-list (directory-files DIRECTORY FULL "[^\.]$" NOSORT))
            files file)
        (dolist (item temp-list)
          (setq file (if FULL item (expand-file-name item DIRECTORY)))
          (cond
           ((file-directory-p file)
            (setq files (append
                         files
                         (directory-files-recursively file FULL MATCH NOSORT (1- MAX-DEPTH)))))
           ((or (not MATCH) (string-match-p MATCH file))
            (add-to-list 'files item))))
        files)))

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))


;; ------ personal interactive functions
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font)) nil t))
(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))
;; (defun qiang-set-font (english-fonts
;;                        &optional
;;                        english-font-size
;;                        chinese-fonts)
;;   "english-font-size could be set to \":pixelsize=18\" or a integer.
;; If set/leave chinese-font-size to nil, it will follow english-font-size"
;;   (if window-system
;;       (let* ((en-font (qiang-make-font-string
;;                        (find-if #'qiang-font-existsp english-fonts)
;;                        english-font-size))
;;              (useable-zh-font (find-if #'qiang-font-existsp chinese-fonts))
;;              (zh-font (font-spec :family useable-zh-font))
;;              (chinese-font-size (list (cons useable-zh-font 1.0))))
;;         (message "Set English Font to %s" en-font)
;;         (set-face-attribute
;;          'default nil :font en-font)
;;         (message "Set Chinese Font to %s" zh-font)
;;         (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;           (set-fontset-font (frame-parameter nil 'font)
;;                             charset
;;                             zh-font))
;;         (setq face-font-rescale-alist chinese-font-size))))

;; refs: http://baohaojun.github.io/perfect-emacs-chinese-font.html
(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-fonts-scale
                       )
  (setq chinese-fonts-scale (or chinese-fonts-scale 1.2))
  (setq face-font-rescale-alist `(("Microsoft Yahei" . ,chinese-fonts-scale)
                                  ("Microsoft_Yahei" . ,chinese-fonts-scale)
                                  ("微软雅黑" . ,chinese-fonts-scale)
                                  ("WenQuanYi Zen Hei" . ,chinese-fonts-scale)))
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts))))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (set-face-attribute
     'default nil :font en-font)
    (condition-case font-error
        (progn
          (set-face-font 'italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'normal :size (+ 0.0 english-font-size)))
          (set-face-font 'bold-italic (font-spec :family "JetBrains Mono" :slant 'italic :weight 'bold :size (+ 0.0 english-font-size)))

          (set-fontset-font t 'symbol (font-spec :family "JetBrains Mono")))
      (error nil))
    (set-fontset-font t 'symbol (font-spec :family "Unifont") nil 'append)
    (set-fontset-font t nil (font-spec :family "DejaVu Sans"))

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset zh-font)))
  (when (and (boundp 'global-emojify-mode)
             global-emojify-mode)
    (global-emojify-mode 1))
  (shell-command-to-string "setsid sawfish-client -e '(maximize-window (input-focus))'"))

(defun my/copy-line (arg)
  "Copy line in the kill ring, With prefix arg will copy whole line include spaces"
  (interactive "P")
  (save-excursion
    (if arg
        (beginning-of-line)
      (beginning-of-line-text))
    (kill-ring-save (point) (line-end-position)))
  (message "the line was copied"))

(defun my/duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun my//get-comment ()
  "Get comment for different modes"
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "//")
   ((string-equal mode-name "Web") "//")
   ((string-equal mode-name "Py") "#")
   ((string-equal mode-name "Erlang") "%%")
   (t (if comment-start comment-start ""))))
(defun my/insert-separator(&optional paragraph-p)
  (save-excursion
    (insert
     (format (if paragraph-p
                 "%s ====== "
               "%s ------ ")
             (my//get-comment))))
  (forward-char (+ (length (my//get-comment)) 8)))


(defun my/buffer-info(short-p)
  "show and copy current buffer info based on major-mode"
  (interactive "P")
  (let ((result nil)
        (mode-str mode-name))
    (cond
     ((string-prefix-p "w3m" mode-str) (setq result (w3m-current-title)))     
     (t (setq result (if short-p
                         (if (projectile-project-p)
                             (replace-regexp-in-string
                              (regexp-quote (projectile-project-p)) "" (buffer-file-name))
                           (buffer-name))
                       (buffer-file-name)))))
    (when result
      (message result)
      (kill-new result))))

(defun my/beginning-of-line (&optional n)
  (interactive "p")
  (let ((curr-point (point)))
    (beginning-of-line-text n)
    (if (= curr-point (point))
        (beginning-of-line))))

(defun my/scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))))

(defun my/quickping (host)
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun my/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my/format-buffer()
  (interactive)
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") (call-interactively 'phpcbf))
   ((string-equal mode-name "Web") "//")
   ((string-equal mode-name "Py") "#")
   ((string-equal mode-name "Erlang") "%%")
   ((memq major-mode '(js2-mode
                       js-mode
                       json-mode)) (web-beautify-js))
   ((memq major-mode '(sgml-mode
                       web-mode)) (web-beautify-html))
   ((eq major-mode 'css-mode) (web-beautify-css))
   (t nil)))

(defun my/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defvar my--hungry-delete-string "[[:space:]\n\t]")
(defun my/contextual-kill-word ()
  "Hungry whitespace or kill word depending on context."
  (interactive)
  (if (looking-at-p my--hungry-delete-string)
      (if (boundp 'hungry-delete-forward)
          (call-interactively 'hungry-delete-forward)
        (while (looking-at-p my--hungry-delete-string)
          (delete-char 1)))
    (cond
     ((and (boundp 'smartparens-strict-mode) smartparens-strict-mode)
      (call-interactively 'sp-kill-word))
     (t (call-interactively 'kill-word)))))

(defun my/contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back my--hungry-delete-string (- (point) 1))
      (if (boundp 'hungry-delete-backward)
          (call-interactively 'hungry-delete-backward)
        (while (looking-back my--hungry-delete-string (- (point) 1))
          (delete-char -1)))
    (cond
     ((and (boundp 'smartparens-strict-mode) smartparens-strict-mode)
      (call-interactively 'sp-backward-kill-word))
     (t (call-interactively 'backward-kill-word)))))

(defun my/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;;; functions.el ends here
