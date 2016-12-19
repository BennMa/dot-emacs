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
  "white list of last buffer switcher"
  :type '(repeat regexp))
(defcustom previous-buffer-white-modes-list '()
  "white list of modes of last buffer switcher"
  :type '(repeat string))
(defcustom previous-buffer-black-list '()
  "black list of last buffer switcher"
  :type '(repeat regexp))
(defcustom previous-buffer-black-modes-list '()
  "black list of modes of last buffer switcher"
  :type '(repeat string))

(defun switch-to-previous-buffer ()
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
                                      previous-buffer-white-list)
                  (member-ignore-case last-buffer-mode
                                      previous-buffer-white-modes-list)
                  (not (or (list-regex-match-p last-buffer-name
                                               previous-buffer-black-list)
                           (member-ignore-case last-buffer-mode
                                               previous-buffer-black-modes-list))))
          (switch-to-buffer last-buffer-name)
          (throw 'matched_ t))
        (setq i (1+ i))
        nil)
      )))

(defun my-other-window (count &optional all-frames)
  (interactive "p")
  (let ((i 1))
    (catch 'matched_
      (while (< i 6)
        (other-window count all-frames)
        (when (not (member major-mode '(direx:direx-mode project-explorer-mode)))
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
  (save-excursion
    (insert
     (format (if paragraph-p
                 "%s ====== "
               "%s ------ ")
             (get-comment))))
  (forward-char (+ (length (get-comment)) 8)))

(defun get-comment ()
  "Get comment for different modes"
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "//")
   ((string-equal mode-name "Web") "//")
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
(defcustom individual-english-fonts '()
  "english fonts of emacs"
  :type '(repeat string))
(defcustom individual-chinese-fonts '()
  "chinese fonts of emacs"
  :type '(repeat string))
(defcustom individual-font-size 14
  "font-size of emacs"
  :type 'integer)

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

(defun my-beginning-of-line (&optional n)
  (interactive "p")
  (let ((curr-point (point)))
    (beginning-of-line-text n)
    (if (= curr-point (point))
        (beginning-of-line))))

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

(defun quickping (host)
  (= 0 (call-process "ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun my-location-switch(&optional location)
  (interactive)
  (let ((target-location (or location (if (string= my-location "Home")
                                          "Office"
                                        "Home"))))
    (setq my-location target-location)
    (async-shell-command (concat "/usr/sbin/scselect " target-location))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "") 
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun keymaps-at-point ()
  "List entire keymaps present at point."
  (interactive)
  (let ((map-list
         (list
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'keymap))
                  (overlays-at (point)))
          (mapcar (lambda (overlay)
                    (overlay-get overlay 'local-map))
                  (overlays-at (point)))
          (get-text-property (point) 'keymap)
          (get-text-property (point) 'local-map))))
    (apply #'message
           (concat 
            "Overlay keymap: %s\n"
            "Overlay local-map: %s\n"
            "Text-property keymap: %s\n"
            "Text-property local-map: %s")
           map-list)))

;; ------ notification
(defvar terminal-notifier-bin
  "/usr/local/bin/terminal-notifier")
(defun send-notification-for-mac (title msg &optional url command group sender activate sound)
  (shell-command (concat terminal-notifier-bin
                         " -message '" msg "'"
                         " -title '" title "'"
                         " -sound " sound
                         " -sender " sender
                         " -activate " activate
                         " -group " group
                         " -open '" url "'"
                         " -execute '" command "'")))

(defvar emacsclient-bin "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")
(defun my-notify(msg callback-command &optional title group)
  (interactive)
  (let ((is_url (string-match-p "^https?\\:\\/\\/" callback-command))
        (group (or group "Emacs"))
        (sender "org.gnu.emacs")
        (activate "org.gnu.emacs")
        (sound "default"))
    (send-notification-for-mac title msg (and is_url callback-command)
                               (and (not is_url) (concat emacsclient-bin " -nqe \"" callback-command "\"")) ;; execute emacs code/function when click the notification
                               group sender activate sound)))

(defun my-code-format()
  (interactive)
  (cond
   ((string-equal mode-name "Emacs-Lisp") ";;")
   ((string-equal mode-name "Lisp") ";;")
   ((string-equal mode-name "PHP") "//")
   ((string-equal mode-name "Web") "//")
   ((string-equal mode-name "Py") "#")
   ((string-equal mode-name "Erlang") "%%")
   ((string-match "^Javascript-IDE" mode-name) (tide-format))
   (t nil)))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; ------ macos terminal clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(unless window-system
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; ------ export
(provide 'my-toolkit)
