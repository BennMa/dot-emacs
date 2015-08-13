;; ------ utility macros
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
or \\[abort-recursive-edit] (abort)), restore window configuration
in current frame.
Inspired by Erik Naggum's `recursive-edit-with-single-window'."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))


;; ------ basic functions
(defun list-regex-match-p (string list)
  (catch 'matched_ (dolist (regex list)
                     (if (string-match regex string)
                         (throw 'matched_ t))) nil))


;; ------ advices
(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))



;; ------ utility functions
(defun my-split-window (number)
  (let* ((W1 (get-buffer-window))
         (windows (list (cons 1 W1)))
         i W2 W3 W4 W5 W6 W7 W8 W9 W10 W11 W12
         curr-window last-window)
    (when (> number 1)
      (setq W2 (split-window W1 nil 'below))
      (add-to-list 'windows (cons 2 W2) t))
    (loop for i from 3 upto number do
          (setq curr-window (intern (concat "W" (int-to-string i))))
          (setq last-window (symbol-value (intern (concat "W"
                                                          (int-to-string (- i 2)))) ))
          (set curr-window (split-window last-window nil 'right))
          (add-to-list 'windows (cons i (symbol-value curr-window)) t)
          (balance-windows) )
    windows))


(autoload 'org-cycle "org" nil t)
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'indent-according-to-mode "indent" nil t)
(defun smart-tab (&optional arg)
  (interactive "P")
  (cond
   ((looking-back "^[-+* \t]*")
    (if (eq major-mode 'org-mode)
        (org-cycle arg)
      (indent-according-to-mode)))
   (t
    ;; Hippie also expands yasnippets, due to `yas-hippie-try-expand' in
    ;; `hippie-expand-try-functions-list'.
    (hippie-expand arg))))


(defcustom previous-buffer-white-list '()
  "white list for filter previous buffer which used by C-TAB"
  :type '(repeat regexp))
(defcustom previous-buffer-black-list '()
  "bloack list for filter previous buffer which used by C-TAB"
  :type '(repeat regexp))
(defun switch-to-previous-buffer ()
  "switch to last active buffer by white list and black list"
  (interactive)
  (let ((i 1)
        (last-buffer-name nil))
    (catch 'matched_
      (while (< i 50)
        (setq last-buffer-name (buffer-name (nth i (buffer-list))))
        (when (or (list-regex-match-p last-buffer-name previous-buffer-white-list)
                  (not (list-regex-match-p last-buffer-name previous-buffer-black-list)))
          (switch-to-buffer last-buffer-name)
          (throw 'matched_ t))
        (setq i (1+ i))))))


(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(defun copy-line (arg)
  "Copy line in the kill ring, With prefix arg will copy whole line include spaces"
  (interactive "P")
  (save-excursion
    (if arg
        (beginning-of-line)
      (beginning-of-line-text))
    (kill-ring-save (point) (line-end-position)))
  (message "the line was copied"))

(defun duplicate-line ()
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

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-char (point-min))
        (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(defun delete-current-buffer-file ()
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


(defun insert-separator(&optional paragraph-p)
  (let ((pos (point)))
    (insert
     (format (if paragraph-p
                 "%s ====== separator"
               "%s ------ separator")
             (get-comment)))
    ;;(goto-char pos)
    (forward-char -9)))

(defun get-comment ()
  "Get comment for different modes"
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "#")
   ((string-equal mode-name "Web") "#")
   ((string-equal mode-name "Py") "#")
   ((string-equal mode-name "Erlang") "%%")
   (t (if comment-start comment-start ""))))


(defun show-magic-info(show-short-info-p)
  "show interesting information for current file or directory
 - If current position is dired, show lines counts for misc programming lanuages.
 - If current position is a file, show file name and line counts
 - If current position is in *w3m*, show current web title
 - If current positioin is in Article mail mode, show the url under current cursor
 If show-short-info-p is not null, the default behaviour is showing the short
 filename with extension removed.
 "
  (interactive "P")
  (let ((output_str buffer-file-name)
        (mode-str mode-name))
    (if (and output_str show-short-info-p)
        (setq output_str
              (if (projectile-project-p)
                  (replace-regexp-in-string
                   (regexp-quote (projectile-project-p)) "" (buffer-file-name))
                (buffer-name))))
    (cond
     ((string-prefix-p "w3m" mode-str) (setq output_str (w3m-current-title)))
     ((string-prefix-p "Dired" mode-str)
      (setq output_str (count-code-lines-in-directory default-directory)))
     ((string-prefix-p "Article" mode-str) (setq output_str (w3m-print-this-url)))
     (t (if (null output_str) (setq output_str (buffer-name))))
     )
    (when output_str
      ;; display the result
      (message output_str)
      ;; send the result to kill-string
      (kill-new output_str))
    ))
(defun count-code-lines-in-directory(directory &optional lanuage-postfix-list)
  " Count code lines for various programming lanuages, with the help of below utility:
 find . -name '%s' | xargs wc -l 2>/dev/null | tail -n 1
 "
  (interactive "Ddirectory:")
  (let (command-output (output-str "Count code lines."))
    ;; set lanuages to be checked, if not given
    (if (null lanuage-postfix-list)
        (setq lanuage-postfix-list
              '("*.php" "*.c" "*.c++" "*.cxx" "*.rb" "*.py" "*.go"
                "*.el" "*.sh" "*.java" "*.pl" "*.erl" "*.cpp" "*.cc" "*.cxx"
                "*.m" "*.h"
                "*.js" "*.sql" "*.mxml" "*.as")))
    ;; count lines
    (dolist (lanuage-var lanuage-postfix-list)
      ;; TODO, remove comments from counting
      ;; suppress the possible stderr for wc, since some temporary files may not be reachable.
      (setq command-output
            (shell-command-to-string
             (format "find . -name '%s' | xargs wc -l 2>/dev/null | tail -n 1" lanuage-var)))
      (unless (or (string= command-output "0\n")
                  (string= command-output ""))
        (setq output-str (format "%s\n%s: %s " output-str lanuage-var command-output))
        ))
    ;; return the result
    (eval output-str)
    ))


(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))



;; ------ font & size related
;; (eval-when-compile
;;   (defvar emacs-min-height)
;;   (defvar emacs-min-width))

(defvar emacs-min-top 23)
(defvar emacs-min-left 0)
(defvar emacs-min-width 100)
(defvar emacs-min-height 40)
(defvar emacs-english-fonts '("Menlo" "Courier New" "Monaco" "Inconsolata"
                              "Anonymous Pro" "Monospace" "Courier"))
(defvar emacs-chinese-fonts '("宋体" "黑体" "新宋体" "文泉驿等宽微米黑" "Microsoft Yahei"))

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font)) nil t))
(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))
(defun qiang-set-font (english-font-size
                       chinese-font-size
                       &optional english-fonts
                       chinese-fonts)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (or english-fonts (setq english-fonts emacs-english-fonts))
  (or chinese-fonts (setq chinese-fonts emacs-chinese-fonts))
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))

(defun emacs-min ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)
  
  (qiang-set-font 15 16))

(defun emacs-max ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)
  
  (qiang-set-font 17 18))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))


;; ------ help related
(defun my-switch-in-other-buffer (buf)
  (when buf
    (split-window-vertically)
    (switch-to-buffer-other-window buf)))

(defun my-describe-symbol  (symbol &optional mode)
  (interactive
   (info-lookup-interactive-arguments 'symbol current-prefix-arg))
  (let (info-buf find-buf desc-buf cust-buf)
    (save-window-excursion
      (ignore-errors
        (info-lookup-symbol symbol mode)
        (setq info-buf (get-buffer "*info*")))
      (let ((sym (intern-soft symbol)))
        (when sym
          (if (functionp sym)
              (progn
                (find-function sym)
                (setq find-buf (current-buffer))
                (describe-function sym)
                (setq desc-buf (get-buffer "*Help*")))
            (find-variable sym)
            (setq find-buf (current-buffer))
            (describe-variable sym)
            (setq desc-buf (get-buffer "*Help*"))
            ;;(customize-variable sym)
            ;;(setq cust-buf (current-buffer))
            ))))

    (delete-other-windows)

    (switch-to-buffer find-buf)
    (my-switch-in-other-buffer desc-buf)
    (my-switch-in-other-buffer info-buf)
    ;;(switch-in-other-buffer cust-buf)
    (balance-windows)))


(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))


(provide 'my-toolkit)
