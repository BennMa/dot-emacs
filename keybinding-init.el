;; Main keymaps for personal bindings are:
;;
;;   C-x <letter>  primary map (has many defaults too)
;;   C-c <letter>  secondary map (not just for mode-specific)
;;   C-. <letter>  tertiary map
;;
;;   M-g <letter>  goto map
;;   M-s <letter>  search map
;;   M-o <letter>  markup map (even if only temporarily)
;;
;;   C-<capital letter>
;;   M-<capital letter>
;;
;;   A-<anything>
;;   M-A-<anything>
;;
;;   M-p-<anything> projectile
;;
;; Single-letter bindings still available:
;;   C- ,'";:?<>|!#$%^&*`~ <tab>
;;   M- ?#

;; ====== global-map

(fset 'yes-or-no-p 'y-or-n-p)
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

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;; ====== C-

(defcustom previous-buffer-white-list '()
  "white list for filter previous buffer which used by C-TAB"
  :type '(repeat regexp))
(defcustom previous-buffer-black-list '()
  "bloack list for filter previous buffer which used by C-TAB"
  :type '(repeat regexp))
(defun switch-to-previous-buffer ()
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
(bind-key "C-<tab>" 'switch-to-previous-buffer)

(defvar ctl-period-map)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." 'ctl-period-map)

(bind-key* "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'delete-other-windows)

;; ------ C-M-

(bind-key "<C-M-backspace>" 'backward-kill-sexp)
(bind-key "C-M-<tab>" 'ido-switch-buffer)

;; ====== M-

(defadvice async-shell-command (before uniqify-running-shell-command activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" 'async-shell-command)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(bind-key "M-[" 'align-code)
(bind-key "M-`" 'other-frame)

(bind-key "M-j" 'delete-indentation-forward)
(bind-key "M-J" 'delete-indentation)

(bind-key "M-W" 'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" 'mark-sentence)
(bind-key "M-X" 'mark-sexp)
(bind-key "M-D" 'mark-defun)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

;; ====== C-x -

(bind-key "C-x d" 'delete-whitespace-rectangle)
(bind-key "C-x F" 'set-fill-column)
(bind-key "C-x t" 'toggle-truncate-lines)

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

(bind-key "C-x K" 'delete-current-buffer-file)

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

(bind-key "C-x y" '(lambda() (interactive) (insert-separator nil)))
(bind-key "C-x Y" '(lambda() (interactive) (insert-separator t)))

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
        (mode-str mode-name)
        )
    (if (and output_str show-short-info-p)
        (setq output_str
              (file-name-sans-extension (file-name-nondirectory output_str))))
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
(bind-key* "C-x v" 'show-magic-info)

;; ------ C-x C-

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

(bind-key "C-x C-d" 'duplicate-line)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-x C-n" 'next-line)

(defun find-alternate-file-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" 'find-alternate-file-with-sudo)

;; ------ C-x M-

(bind-key "C-x M-n" 'set-goal-column)

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

(bind-key "C-x M-q" 'refill-paragraph)

;; ====== mode-specific-map, C-c

(bind-key "C-c <tab>" 'ff-find-other-file)
(bind-key "C-c SPC" 'just-one-space)

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

(bind-key "C-c 0"
  (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
  (recursive-edit-preserving-window-config
   (if (one-window-p 'ignore-minibuffer)
       (error "Current window is the only window in its frame")
     (delete-other-windows))))

(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" 'delete-current-line)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

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

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-char (point-min))
        (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))

(bind-key "C-c g" 'goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(bind-key "C-c k" 'keep-lines)


;; ------ emacs frame init, and font setting
(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(when window-system
  (defvar emacs-min-top 23)
  (defvar emacs-min-left 0)
  (defvar emacs-min-width 100)
  (defvar emacs-min-height 40)
  (let ((frame-alist
         (list (cons 'top    emacs-min-top)
               (cons 'left   emacs-min-left)
               (cons 'height emacs-min-height)
               (cons 'width  emacs-min-width))))
    (setq initial-frame-alist frame-alist))  

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
    (or english-fonts (setq english-fonts '("Menlo" "Courier New" "Monaco"
    "Inconsolata" "Anonymous Pro" "Monospace" "Courier")))
    (or chinese-fonts (setq chinese-fonts '("宋体" "黑体" "新宋体" "文泉驿等宽微米黑" "Microsoft Yahei")))
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

  (if window-system
      (add-hook 'after-init-hook 'emacs-min))

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

  (bind-key "C-c m" 'emacs-toggle-size))

(bind-key "C-c o" 'customize-option)
(bind-key "C-c O" 'customize-group)
(bind-key "C-c F" 'customize-face)

(bind-key "C-c q" 'fill-region)
(bind-key "C-c r" 'replace-regexp)
(bind-key "C-c s" 'replace-string)
(bind-key "C-c u" 'rename-uniquely)

(bind-key "C-c v" 'ffap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

(bind-key "C-c V" 'view-clipboard)
(bind-key "C-c z" 'clean-buffer-list)

(bind-key "C-c [" 'align-regexp)
(bind-key "C-c =" 'count-matches)
(bind-key "C-c ;" 'comment-or-uncomment-region)

;; ------ C-c C-

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" 'delete-to-end-of-buffer)

;; ------ C-c M-

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

(bind-key "C-c M-q" 'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

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

(bind-key "C-h e d" 'my-describe-symbol)
(bind-key "C-h e i" 'info-apropos)
(bind-key "C-h e k" 'find-function-on-key)
(bind-key "C-h e l" 'find-library)

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

(bind-key "C-h e s" 'scratch)
(bind-key "C-h e v" 'find-variable)
(bind-key "C-h e V" 'apropos-value)
