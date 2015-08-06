(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fit-frame-flag nil)
 '(abbrev-file-name "~/.emacs.d/abbrevs")
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start 3)
 '(ac-comphist-file "~/.emacs.d/data/ac-comphist.dat")
 '(ac-dwim nil)
 '(ac-ignore-case nil)
 '(ac-trigger-key "<tab>")
 '(ac-use-fuzzy nil)
 '(ace-isearch-submode (quote ace-jump-char-mode))
 '(ad-redefinition-action (quote accept))
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.5)
 '(alert-default-style (quote fringe))
 '(alert-notifier-command
   "/Applications/Misc/terminal-notifier.app/Contents/MacOS/terminal-notifier")
 '(align-c++-modes (quote (csharp-mode c++-mode c-mode java-mode groovy-mode)))
 '(align-to-tab-stop nil)
 '(allout-command-prefix ".")
 '(ansi-color-names-vector
   ["black" "red" "green" "brown" "blue" "magenta" "blue" "white"])
 '(appt-display-interval 30)
 '(appt-message-warning-time 60)
 '(auth-source-save-behavior nil)
 '(auto-compression-mode t nil (jka-compr))
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:.*" "/tmp" t))))
 '(auto-save-interval 64)
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-save-list/.saves-")
 '(auto-save-timeout 2)
 '(backup-directory-alist
   (quote
    (("/Volumes/Files/" . "/Volumes/Files/.backups")
     ("\\(recentf\\|archive/sent\\)" . "/tmp")
     (".*" . "~/.backups"))))
 '(backward-delete-char-untabify-method (quote untabify))
 '(bbdb-default-country "")
 '(bbdb-file "~/Documents/bbdb")
 '(bbdb-message-caching-enabled nil)
 '(bbdb-no-duplicates t)
 '(bbdb-offer-save (quote savenoprompt))
 '(bbdb-silent-running t)
 '(bbdb-use-pop-up nil)
 '(bbdb-vcard-import-translation-table
   (quote
    (("CELL\\|CAR" . "Mobile")
     ("WORK" . "Work")
     ("HOME" . "Home")
     ("^$" . "Work"))))
 '(bbdb/mail-auto-create-p nil)
 '(bc-bookmark-file "~/.emacs.d/data/breadcrumb")
 '(bind-key-segregation-regexp "\\`\\(\\(C-[chx.] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)")
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bmkp-bmenu-commands-file "~/.emacs.d/data/bmk-bmenu-commands.el")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-crosshairs-flag nil)
 '(bmkp-last-as-first-bookmark-file "~/Documents/bookmarks")
 '(bookmark-default-file "~/Documents/bookmarks")
 '(browse-url-browser-function (quote ((".*" . browse-url-default-macosx-browser))))
 '(byte-compile-verbose nil)
 '(c-default-style
   (quote
    ((java-mode . "gnu")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(calendar-daylight-time-zone-name "CDT")
 '(calendar-latitude 40.73471)
 '(calendar-longitude -89.554659)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "CST")
 '(calendar-time-zone -420)
 '(canlock-password "8d2ee9a7e4658c4ff6d863f91a3dd5340b3918ec")
 '(cc-other-file-alist
   (quote
    (("\\.hs\\'"
      (".hs-boot"))
     ("\\.cc\\'"
      (".hh" ".h"))
     ("\\.hh\\'"
      (".cc" ".C"))
     ("\\.c\\'"
      (".h"))
     ("\\.h\\'"
      (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
     ("\\.C\\'"
      (".H" ".hh" ".h"))
     ("\\.H\\'"
      (".C" ".CC"))
     ("\\.CC\\'"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH\\'"
      (".CC"))
     ("\\.c\\+\\+\\'"
      (".h++" ".hh" ".h"))
     ("\\.h\\+\\+\\'"
      (".c++"))
     ("\\.cpp\\'"
      (".hpp" ".hh" ".h"))
     ("\\.hpp\\'"
      (".cpp"))
     ("\\.cxx\\'"
      (".hxx" ".hh" ".h"))
     ("\\.hxx\\'"
      (".cxx")))))
 '(cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "*Group*" "*Org Agenda*" "todo.txt" "&bitlbee")))
 '(clean-buffer-list-kill-never-regexps
   (quote
    ("^ \\*Minibuf-.*\\*$" "^\\*Summary" "^\\*Article" "^#")))
 '(clean-buffer-list-kill-regexps (quote (".*")))
 '(column-number-mode t)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-frontend)))
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compilation-search-path
   (quote
    (nil "~/src/gitlib" "~/src/gitlib/gitlib" "~/src/gitlib/gitlib-libgit2" "~/src/gitlib/gitlib-s3" "~/src/gitlib/gitlib-test" "~/src/gitlib/git-monitor" "~/src/c2hsc")))
 '(compilation-skip-threshold 2)
 '(compilation-window-height 100)
 '(coq-compile-before-require t)
 '(coq-holes-minor-mode nil)
 '(coq-maths-menu-enable t)
 '(coq-one-command-per-line nil)
 '(coq-prog-args (quote ("-emacs" "-dont-load-proofs")))
 '(coq-prog-name "ssrcoq")
 '(current-language-environment "UTF-8")
 '(cursor-type (quote bar))
 '(custom-buffer-done-function (quote kill-buffer))
 '(custom-file "~/.emacs.d/custom-settings.el")
 '(custom-raised-buttons nil)
 '(default-frame-alist (quote ((cursor-color . "#CCC"))))
 '(default-major-mode (quote text-mode) t)
 '(deft-auto-save-interval 0.0)
 '(deft-directory "~/Documents/notes")
 '(deft-text-mode (quote org-mode))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions (quote none))
 '(delete-selection-mode t)
 '(diary-file "~/Documents/diary")
 '(diff-mode-hook
   (quote
    (diff-delete-empty-files diff-make-unified smerge-mode)))
 '(directory-free-space-args "-kh")
 '(dired-clean-up-buffers-too nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-lah")
 '(dired-no-confirm
   (quote
    (byte-compile chgrp chmod chown copy hardlink symlink touch)))
 '(dired-omit-files
   "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\)$\\|^\\.\\.$")
 '(dired-omit-mode nil t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(diredful-init-file "~/.emacs.d/data/diredful-conf.el")
 '(doc-view-resolution 300)
 '(ediff-combination-pattern
   (quote
    ("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))
 '(ediff-diff-options "-w")
 '(ediff-highlight-all-diffs nil)
 '(ediff-show-clashes-only t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(edit-server-new-frame nil)
 '(el-get-auto-update-cached-recipes nil)
 '(el-get-dir "~/.emacs.d/site-lisp")
 '(el-get-generate-autoloads nil)
 '(el-get-use-autoloads nil)
 '(electric-indent-mode nil)
 '(emacs-lisp-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function))))
     eldoc-mode
     (lambda nil
       (local-set-key
        [(meta 46)]
        (quote find-function))
       (local-set-key
        [(control 109)]
        (quote newline-and-indent))))))
 '(enable-recursive-minibuffers t)
 '(erc-auto-query (quote window-noselect))
 '(erc-autoaway-message "I'm away (after %i seconds of idle-time)")
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "##categorytheory" "#agda" "#coq" "#idris" "#gitlib" "#haskell" "#haskell-lens" "#haskell-emacs" "#ledger" "#ChicagoHaskell" "#haskell-ops" "#nixos" "##nix-darwin" "#wigfam" "##hott" "#ghc" "#haskell-infrastructure" "#org-mode" "#haskell.org"))))
 '(erc-fill-function (quote erc-fill-variable))
 '(erc-fill-static-center 12)
 '(erc-foolish-content (quote ("MichaelSnoyman" "BrendanHay")))
 '(erc-format-nick-function (quote erc-format-@nick))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-short))
 '(erc-header-line-format nil)
 '(erc-hide-list (quote ("JOIN" "NICK" "PART" "QUIT" "MODE")))
 '(erc-keywords (quote ("wiegley" "ledger" "eshell")))
 '(erc-log-channels-directory "~/Messages/ERC")
 '(erc-log-write-after-send t)
 '(erc-modules
   (quote
    (autojoin button completion dcc fill identd irccontrols list match menu move-to-prompt netsplit networks noncommands readonly replace ring scrolltobottom services smiley stamp track truncate highlight-nicknames)))
 '(erc-nick "johnw")
 '(erc-port 6667)
 '(erc-priority-people-regexp "\\`[^#].+")
 '(erc-prompt-for-nickserv-password nil)
 '(erc-replace-alist (quote (("</?FONT>" . ""))))
 '(erc-server "irc.freenode.net")
 '(erc-services-mode t)
 '(erc-text-matched-hook (quote (erc-hide-fools my-erc-hook)))
 '(erc-track-enable-keybindings t)
 '(erc-track-exclude
   (quote
    ("#idris" "#emacs" "#ghc" "#haskell-emacs" "#agda" "#twitter_jwiegley")))
 '(erc-track-exclude-types
   (quote
    ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
 '(erc-track-faces-priority-list
   (quote
    (erc-error-face
     (erc-nick-default-face erc-current-nick-face)
     erc-current-nick-face erc-keyword-face
     (erc-nick-default-face erc-pal-face)
     erc-pal-face erc-nick-msg-face erc-direct-msg-face)))
 '(erc-track-score-mode t)
 '(erc-track-showcount t)
 '(erc-user-full-name (quote user-full-name))
 '(erc-yank-query-before-gisting nil)
 '(eshell-directory-name "~/.emacs.d/eshell/")
 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (abbreviate-file-name
       (eshell/pwd))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands
   (quote
    ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))
 '(eudc-inline-expansion-format (quote ("%s <%s>" name email)))
 '(eval-expr-print-function (quote pp))
 '(eww-lnum-actions-link-alist
   (quote
    ("----  Link   ----"
     (102 eww-lnum-visit "Visit")
     (101
      (lambda
        (info)
        (eww-lnum-visit info nil t))
      "Edit URL and visit")
     (70
      (lambda
        (info)
        (eww-lnum-visit info t))
      "Visit in new buffer")
     (69
      (lambda
        (info)
        (eww-lnum-visit info t t))
      "Edit URL and visit in new buffer")
     (98
      (lambda
        (info)
        (eww-lnum-visit info :background))
      "Open in background")
     (66
      (lambda
        (info)
        (eww-lnum-visit info :background t))
      "Edit URL and open in background")
     (100
      (lambda
        (info)
        (save-excursion
          (goto-char
           (cadr info))
          (eww-download)))
      "Download")
     (119
      (lambda
        (info)
        (let
            ((url
              (car info)))
          (kill-new url)
          (message url)))
      "Copy")
     (38
      (lambda
        (info)
        (eww-browse-with-external-browser
         (car info)))
      "Open in external browser")
     (68
      (lambda
        (info)
        (shell-command
         (concat "aria2c -d ~/Downloads -x5 '"
                 (car info)
                 "' &")
         "*Aria*"))
      "Download with Aria"))))
 '(eww-search-prefix "https://startpage.com/do/m/mobilesearch?query=")
 '(fill-column 78)
 '(find-ls-option (quote ("-print0 | xargs -0 ls -ld" . "-ld")))
 '(find-ls-subdir-switches "-alh")
 '(flx-ido-use-faces nil)
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-standard-error-navigation nil)
 '(flymake-compilation-prevents-syntax-check nil)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(fold-this-persistent-folds-file "~/.emacs.d/data/fold-this.el")
 '(font-lock-support-mode (quote jit-lock-mode))
 '(font-lock-verbose nil)
 '(frame-title-format
   (quote
    (:eval
     (concat
      (if buffer-file-name default-directory "%b")
      "    "
      (number-to-string
       (cdr
        (assq
         (quote width)
         (frame-parameters))))
      "x"
      (number-to-string
       (cdr
        (assq
         (quote height)
         (frame-parameters))))))) t)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-same-frame nil)
 '(ggtags-enable-navigation-keys nil)
 '(ggtags-oversize-limit 1048576)
 '(ggtags-use-sqlite3 t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-undo-tree-mode t)
 '(grep-find-command (quote ("ag --noheading --column --ignore branches " . 43)))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "cache")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "TAG" "#*" "~*")))
 '(helm-adaptive-history-file "~/.emacs.d/data/helm-adaptive-history")
 '(helm-ag-always-set-extra-option nil)
 '(helm-ag-base-command "/usr/local/bin/ag --nocolor --nogroup --smart-case")
 '(helm-ag-ignore-patterns nil)
 '(helm-ag-insert-at-point nil)
 '(helm-ag-use-agignore t)
 '(helm-ag-use-grep-ignore-list t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (ffap)
     (tmm-menubar))))
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-projectile-fuzzy-match nil)
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(helm-split-window-default-side (quote right))
 '(hippie-expand-try-functions-list
   (quote
    (yas-hippie-try-expand try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ibuffer-default-display-maybe-show-predicates t)
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 16 -1)
           " "
           (size 6 -1 :right)
           " "
           (mode 16 16)
           " " filename)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-maybe-show-regexps nil)
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Helm"
       (mode . helm-mode))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)))
      ("Haskell"
       (or
        (mode . haskell-mode)
        (mode . haskell-cabal-mode)
        (mode . literate-haskell-mode)))
      ("Coq"
       (mode . coq-mode))
      ("C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired"
       (mode . dired-mode))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.newsrc-dribble")))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^diary$")
        (mode . org-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-shrink-to-minimum-size t t)
 '(ibuffer-use-other-window t)
 '(icicle-Completions-text-scale-decrease 0)
 '(icicle-apropos-cycle-next-keys (quote ([next] [(control 110)])))
 '(icicle-apropos-cycle-previous-keys (quote ([prior] [(control 112)])))
 '(icicle-incremental-completion nil)
 '(icicle-max-candidates 100)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-cannot-complete-command (quote ido-exit-minibuffer))
 '(ido-confirm-unique-completion t)
 '(ido-decorations
   (quote
    ("{" "}" "," ",..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-enter-matching-directory (quote first))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store" "\\`\\.localized" "\\.sparsebundle/" "\\.dmg\\'")))
 '(ido-save-directory-list-file "~/.emacs.d/data/ido.last")
 '(ido-use-virtual-buffers t)
 '(ido-use-virtual-buffers-automatically t)
 '(idris-interpreter-flags (quote ("-p" "effects")))
 '(image-dired-dir "~/.emacs.d/data/image-dired/")
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message "benn")
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)" "~/.emacs.d/gnus-settings.el" nil nil)
     ("\\`\\(org-\\)" "~/.emacs.d/org-settings.el" nil nil))))
 '(ispell-extra-args (quote ("--sug-mode=fast" "--keyboard=dvorak")))
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-auto-revert-mode nil)
 '(magit-backup-mode t)
 '(magit-completing-read-function (quote helm--completing-read-default))
 '(magit-diff-options nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-process-popup-time 15)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(magit-use-overlays nil)
 '(menu-bar-mode nil)
 '(moccur-following-mode-toggle nil)
 '(modelinepos-column-limit 80)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(mudel-mode-hook (quote (mudel-add-scroll-to-bottom)))
 '(mudel-output-filter-functions (quote (ansi-color-process-output)))
 '(multi-term-program "/usr/bin/screen")
 '(multi-term-program-switches "-DR")
 '(multi-term-scroll-show-maximum-output t)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier (quote alt))
 '(ns-command-modifier (quote meta))
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(olivetti-hide-mode-line t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("ELPA" . "http://tromey.com/elpa/")
     ("Marmalade" . "http://marmalade-repo.org/packages/"))))
 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 '(pcomplete-compare-entries-function (quote file-newer-than-file-p))
 '(persistent-scratch-file-name "~/.emacs.d/data/persistent-scratch")
 '(powerline-default-separator (quote box))
 '(pp^L-^L-string
   "                                                                              ")
 '(previous-buffer-black-list (quote ("^[	 ]*\\*.*\\*[	 ]*$" "collected.org")))
 '(previous-buffer-white-list
   (quote
    ("\\*mu4e-\\(main\\|headers\\)\\*" "\\*\\(?:unsent .*\\|Group\\|Article .*\\|Summary .*\\)\\*" "\\*terminal.*\\*" "\\*magit:[	 ]*.*\\*" "\\*Tail:.*" "\\*erlang.*\\*" "\\*Customize.*\\*")))
 '(projectile-cache-file "~/.emacs.d/data/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-files (quote ("TAGS" "GPATH" "GRTAGS" "GTAGS" "ID")))
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(projectile-switch-project-action (quote helm-projectile))
 '(ps-font-size (quote (8 . 10)))
 '(ps-footer-font-size (quote (12 . 14)))
 '(ps-header-font-size (quote (12 . 14)))
 '(ps-header-title-font-size (quote (14 . 16)))
 '(ps-line-number-font-size 10)
 '(ps-print-color-p nil)
 '(rdebug-many-windows nil)
 '(read-buffer-function (quote ido-read-buffer))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
   (quote
    ("~\\'" "\\`out\\'" "\\.log\\'" "^/[^/]*:" "\\.el\\.gz\\'")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(redisplay-dont-pause t t)
 '(regex-tool-backend (quote perl))
 '(safe-local-variable-values (quote ((after-save-hook git-commit-changes))))
 '(sage-view-anti-aliasing-level 4)
 '(sage-view-margin (quote (20 . 20)))
 '(sage-view-scale 2.0)
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-kill-file-name "~/.emacs.d/data/kill-ring-saved.el")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-margin 0)
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb")
 '(sentence-end-double-space nil)
 '(session-globals-exclude (quote (load-history flyspell-auto-correct-ring)))
 '(session-globals-include
   (quote
    ((kill-ring 10 nil)
     (session-file-alist 200 t)
     (file-name-history 200 nil)
     search-ring regexp-search-ring sr-history-registry)))
 '(session-initialize (quote (session places keys)))
 '(session-name-disable-regexp "\\(\\`/tmp\\|COMMIT_EDITMSG\\)")
 '(session-registers (quote (t (0 . 127))))
 '(session-save-file "~/.emacs.d/data/session")
 '(shm-auto-insert-bangs nil)
 '(shm-indent-spaces 4)
 '(shm-use-hdevtools t)
 '(shm-use-presentation-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote parentheses))
 '(slime-kill-without-query-p t)
 '(slime-repl-history-file "~/.emacs.d/data/slime-history.eld")
 '(slime-startup-animation nil)
 '(smart-compile-alist
   (quote
    (((lambda
        (buf)
        (let
            ((case-fold-search t))
          (and
           (string-match "/ansi/"
                         (buffer-file-name buf))
           (string-match "/opencl"
                         (shell-command-to-string "git symbolic-ref HEAD")))))
      . "cd ~/Contracts/TI/src/c60_iaansi ; ~/Contracts/TI/bin/build.sh c60")
     ((lambda
        (buf)
        (let
            ((case-fold-search t))
          (and
           (string-match "/\\(src/ansi\\|src/.*?ansi\\)/"
                         (buffer-file-name buf))
           (string-match "/merge_4_2"
                         (shell-command-to-string "git symbolic-ref HEAD")))))
      . "cd ~/Contracts/TI/src/msp_iaansi ; ~/Contracts/TI/bin/build.sh msp")
     ((lambda
        (buf)
        (string-match "/\\(\\(src\\|Projects\\)/ledger\\)/"
                      (buffer-file-name buf)))
      . "cd ~/src/ledger ; ~/src/ledger/tools/build.sh debug")
     ((lambda
        (buf)
        (string-match "/emacs/"
                      (buffer-file-name buf)))
      . "emacs-build release macport opt make")
     (emacs-lisp-mode emacs-lisp-byte-compile-and-load)
     ((lambda
        (buf)
        (string-match "/\\(cree\\|EDG/Projects/edg\\)/"
                      (buffer-file-name buf)))
      . "cd ~/Products/cree/edg && (ninja && ctest -j$(ncpu)) & (cd ~/src/cree; mktags src ext/llvm) & wait"))))
 '(sp-highlight-pair-overlay nil)
 '(sql-sqlite-program "sqlite3")
 '(sr-attributes-display-mask (quote (nil nil t nil nil nil)))
 '(sr-autoload-extensions nil)
 '(sr-kill-unused-buffers nil)
 '(sr-listing-switches "--time-style=locale --group-directories-first -alDhgG")
 '(sr-loop-use-popups nil)
 '(sr-popviewer-style (quote single-frame))
 '(sr-show-file-attributes nil)
 '(sr-show-hidden-files t)
 '(sr-use-commander-keys nil)
 '(sr-windows-default-ratio 80)
 '(ssl-certificate-verification-policy 1)
 '(svn-status-hide-unmodified t)
 '(tags-apropos-verbose t)
 '(tags-case-fold-search nil)
 '(tail-max-size 25)
 '(tail-volatile nil)
 '(temp-buffer-resize-mode t nil (help))
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-b" . my-term-send-raw-at-prompt)
     ("C-f" . my-term-send-raw-at-prompt)
     ("C-a" . my-term-send-raw-at-prompt)
     ("C-e" . my-term-send-raw-at-prompt)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M->" . my-term-end-of-buffer)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-d" . term-send-forward-kill-word)
     ("M-DEL" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)
     ("C-y" . term-paste))))
 '(term-buffer-maximum-size 0)
 '(term-scroll-show-maximum-output t)
 '(text-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function)))))))
 '(tls-program
   (quote
    ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CApath /etc/postfix/certs -cert ~/Messages/me.pem")))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.backups")
 '(tramp-default-method "ssh")
 '(tramp-default-method-alist
   (quote
    (("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "sudo"))))
 '(tramp-persistency-file-name "~/.emacs.d/data/tramp")
 '(trash-directory "~/.Trash")
 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.backups"))))
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(url-cache-directory "~/.emacs.d/data/url/cache")
 '(url-configuration-directory "~/.emacs.d/data/url/")
 '(url-irc-function (quote url-irc-erc))
 '(user-full-name "benn")
 '(user-mail-address "sjembn@gmail.com")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell nil)
 '(w3m-cookie-accept-bad-cookies (quote ask))
 '(w3m-default-display-inline-images t)
 '(w3m-fill-column 80)
 '(w3m-use-cookies t)
 '(warning-minimum-log-level :error)
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(weblogger-config-alist
   (quote
    (("newartisans" "http://www.newartisans.com/xmlrpc.php" "johnw" "" "5"))))
 '(wg-mode-line-on nil)
 '(wg-morph-on nil)
 '(wg-prefix-key "")
 '(wg-query-for-save-on-emacs-exit nil)
 '(wg-query-for-save-on-workgroups-mode-exit nil)
 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 80)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(workgroups-mode nil)
 '(x-select-enable-clipboard t)
 '(x-stretch-cursor t)
 '(yaoddmuse-browse-function (quote w3m-browse-url))
 '(yaoddmuse-directory "~/.emacs.d/doc")
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 '(zencoding-preview-default nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((background dark)) (:foreground "#FFFF9B9BFFFF")) (t (:foreground "DarkGreen"))))
 '(diff-changed ((((background dark)) (:foreground "Yellow")) (t (:foreground "MediumBlue"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :weight normal)))))
