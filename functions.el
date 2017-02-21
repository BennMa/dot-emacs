;;; init.el
;;; Commentary:
;;

;;; Code:

;; ------ utility functions
(defsubst add-hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

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
(defun qiang-set-font (english-fonts
                       &optional
                       english-font-size
                       chinese-fonts)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (if window-system
      (let* ((en-font (qiang-make-font-string
                       (find-if #'qiang-font-existsp english-fonts)
                       english-font-size))
             (useable-zh-font (find-if #'qiang-font-existsp chinese-fonts))
             (zh-font (font-spec :family useable-zh-font))
             (chinese-font-size (list (cons useable-zh-font 1.0))))
        (message "Set English Font to %s" en-font)
        (set-face-attribute
         'default nil :font en-font)
        (message "Set Chinese Font to %s" zh-font)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            zh-font))
        (setq face-font-rescale-alist chinese-font-size))))

(defun blaine/copy-line (arg)
  "Copy line in the kill ring, With prefix arg will copy whole line include spaces"
  (interactive "P")
  (save-excursion
    (if arg
        (beginning-of-line)
      (beginning-of-line-text))
    (kill-ring-save (point) (line-end-position)))
  (message "the line was copied"))

(defun blaine/duplicate-line ()
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

(defun blaine//get-comment ()
  "Get comment for different modes"
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "//")
   ((string-equal mode-name "Web") "//")
   ((string-equal mode-name "Py") "#")
   ((string-equal mode-name "Erlang") "%%")
   (t (if comment-start comment-start ""))))
(defun blaine/insert-separator(&optional paragraph-p)
  (save-excursion
    (insert
     (format (if paragraph-p
                 "%s ====== "
               "%s ------ ")
             (blaine//get-comment))))
  (forward-char (+ (length (blaine//get-comment)) 8)))


(defun blaine/buffer-info(short-p)
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

(defun blaine/beginning-of-line (&optional n)
  (interactive "p")
  (let ((curr-point (point)))
    (beginning-of-line-text n)
    (if (= curr-point (point))
        (beginning-of-line))))

(defun blaine/scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))))

(defun blaine/quickping (host)
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun blaine/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun blaine/format-buffer()
  (interactive)
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "//")
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

(defun blaine/kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

(defun blaine/contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode) 
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

;;; functions.el ends here
