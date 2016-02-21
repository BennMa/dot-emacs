;;; Code:

(deftheme my-custom
  "My custom theme.")

;; (load-theme 'my-custom t)

;; Color codes :
;; - blue :       "dodger blue"
;; - yellow :     "#edd400"
;; - green :      "#6ac214"
;; - orange/red : "tomato"
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'my-custom
   `(default ((t (:foreground "#F8F8F2" :background "#1B1D1E")))) ;; #F8F8F2
   `(cursor ((,class (:background "white"))))   
   `(bold ((t (:weight bold))))
   `(bold-italic ((t (:weight bold :slant italic))))
   `(custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
   `(custom-state ((t (:foreground "#A6E22E"))))
   `(italic ((t (:slant italic))))
   `(region ((t (:background "#403D3D"))))
   `(underline ((t (:underline t))))
   `(css-selector ((t (:foreground "#F92672"))))
   `(css-property ((t (:foreground "#66D9EF"))))
   `(diff-added ((t (:foreground "#A6E22E" :weight bold))))
   `(diff-context ((t (:foreground "#F8F8F2"))))
   `(diff-file-header ((t (:foreground "#66D9EF" :background nil))))
   `(diff-indicator-added ((t (:foreground "#A6E22E"))))
   `(diff-indicator-removed ((t (:foreground "#F92672"))))
   `(diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
   `(diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
   `(diff-removed ((t (:foreground "#F92672" :weight bold))))
   `(escape-glyph ((t (:foreground "#E6DB74"))))
   `(minibuffer-prompt ((t (:foreground "#66D9EF"))))
   `(mode-line ((t (:foreground "#F8F8F2" :background "#ff0000"
                               :box (:line-width 1 :color "#ff0000" :style released-button)))))
   `(mode-line-buffer-id ((t (:foreground nil :background nil :weight semi-bold))))
   `(mode-line-inactive ((t (:foreground "#BCBCBC" :background "#1E1C1B"
                                        :box (:line-width 1 :color "#232526")))))
   `(mode-line-mousable ((t (:foreground "#BCBCBC" :background "#ff0000"))))
   `(mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))
   `(font-lock-builtin-face ((t (:foreground "#A6E22E"))))
   `(font-lock-comment-face ((t (:foreground "#465457" :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground "#465457" :slant italic))))
   `(font-lock-constant-face ((t (:foreground "#AE81FF"))))
   `(font-lock-doc-face ((t (:foreground "#E6DB74" :slant italic))))
   `(font-lock-function-name-face ((t (:foreground "#57d001")))) ;; #F92672  :height 160
   `(font-lock-keyword-face ((t (:foreground "#66D9EF"))))
   `(font-lock-negation-char-face ((t (:weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground "#A6E22E"))))
   `(font-lock-regexp-grouping-backslash ((t (:weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold))))
   `(font-lock-string-face ((t (:foreground "#E6DB74"))))
   `(font-lock-type-face ((t (:foreground "#66D9EF")))) ;; #66D9EF
   `(font-lock-variable-name-face ((t (:foreground "#F92672"))))
   `(font-lock-warning-face ((t (:foreground "#FFFFFF"
                                            :background "#333333"))))
   `(fringe ((t (:background "#232526"))))
   `(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
   `(hl-line ((t (:background "#293739"))))
   `(icompletep-choices ((t (:foreground "#F92672"))))
   `(icompletep-determined ((t (:foreground "#A6E22E"))))
   `(icompletep-keys ((t (:foreground "#F92672"))))
   `(icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
   `(isearch ((t (:foreground "#C4BE89" :background "#000000"))))
   `(isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
   `(lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
   `(markdown-italic-face ((t (:slant italic))))
   `(markdown-bold-face ((t (:weight bold))))
   `(markdown-header-face ((t (:weight normal))))
   `(markdown-header-face-1 ((t (:foreground "#66D9EF"))))
   `(markdown-header-face-2 ((t (:foreground "#F92672"))))
   `(markdown-header-face-3 ((t (:foreground "#A6E22E"))))
   `(markdown-header-face-4 ((t (:foreground "#AE81FF"))))
   `(markdown-header-face-5 ((t (:foreground "#E6DB74"))))
   `(markdown-header-face-6 ((t (:foreground "#66D9EF"))))
   `(markdown-inline-code-face ((t (:foreground "#66D9EF"))))
   `(markdown-list-face ((t (:foreground "#A6E22E"))))
   `(markdown-blockquote-face ((t (:slant italic))))
   `(markdown-pre-face ((t (:foreground "#AE81FF"))))
   `(markdown-link-face ((t (:foreground "#66D9EF"))))
   `(markdown-reference-face ((t (:foreground "#66D9EF"))))
   `(markdown-url-face ((t (:foreground "#E6DB74"))))
   `(markdown-link-title-face ((t (:foreground "#F92672"))))
   `(markdown-comment-face ((t (:foreground "#465457"))))
   `(markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
   `(mumamo-background-chunk-major ((t (:background "#272822"))))
   `(mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
   `(outline-1 ((t (:foreground "#66D9EF"))))
   `(outline-2 ((t (:foreground "#F92672"))))
   `(outline-3 ((t (:foreground "#A6E22E"))))
   `(outline-4 ((t (:foreground "#AE81FF"))))
   `(outline-5 ((t (:foreground "#E6DB74"))))
   `(outline-6 ((t (:foreground "#66D9EF"))))
   `(outline-7 ((t (:foreground "#F92672"))))
   `(outline-8 ((t (:foreground "#A6E22E"))))
   `(secondary-selection ((t (:background "#272822"))))
   `(show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
   `(show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
   `(widget-inactive-face ((t (:background "#ff0000"))))
   `(woman-addition ((t (:foreground "#AE81FF"))))
   `(woman-bold ((t (:foreground "#F92672"))))
   `(woman-italic ((t (:foreground "#A6E22E"))))
   '`(woman-unknown ((t (:foreground "#66D9EF"))))

   ;; ------ org
   `(org-default ((t (:inherit default))))
   `(org-hide ((t (:foreground "#2e3436"))))
   `(org-level-1 ((t (:bold t :foreground "dodger blue" :height 1.1 :box))))
   `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.0 :box))))
   `(org-level-3 ((t (:bold nil :foreground "#6ac214" :box))))
   `(org-level-4 ((t (:bold nil :foreground "tomato"))))
   `(org-document-title ((t (:inherit org-level-1 :height 1.2 :underline nil))))
   `(org-date ((t (:underline t :foreground "magenta3"))))
   `(org-footnote  ((t (:underline t :foreground "magenta3"))))
   `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
   `(org-special-keyword ((t (:foreground "brown"))))
   `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
   `(org-block ((t (:foreground "#bbbbbc" :box nil))))
   `(org-checkbox ((t (:foreground "#000000", :background "#93a1a1" :box
                                  (:color "#93a1a1" :style "released-button")))))
   `(org-headline-done ((t (:strike-through t))))
   `(org-quote ((t (:inherit org-block :slant italic))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-todo ((t (:bold t :foreground "Red"))))
   `(org-done ((t (:bold t :foreground "ForestGreen"))))
   ;; (org-tag ((t (:bold t :foreground "ForestGreen"))))
   `(org-habit-alert-face ((((background light)) (:background "#f5f946"))))
   `(org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
   `(org-habit-clear-face ((((background light)) (:background "#8270f9"))))
   `(org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
   `(org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
   `(org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
   `(org-habit-ready-face ((((background light)) (:background "#4df946"))))
   `(org-habit-ready-future-face ((((background light)) (:background
   '                                                     "#acfca9"))))
   `(org-agenda-structure ((t (:inherit default :height 1.2 :underline nil :foreground "tomato"))))
   `(org-agenda-date ((t (:foreground "#6ac214" :height 1.3))))
   `(org-agenda-date-weekend ((t (:foreground "dodger blue" :height 1.3))))
   `(org-agenda-date-today ((t (:foreground "#edd400" :height 1.3))))
   `(org-upcoming-deadline ((t (:foreground "#FF6600"))))
   `(org-scheduled-previously ((t (:foreground "#d6e4fc"))))
   `(org-scheduled-today ((t (:foreground "#FFFFFF"))))
   `(org-agenda-dimmed-todo-face ((t (nil))))
   `(org-priority ((t (nil))))
   ;; (org-agenda-calendar-event ((t (:inherit default :height 1.2))))
   ;; (org-agenda-calendar-sexp ((t (:inherit default :height 1.2))))
   ;; (org-agenda-diary ((t (:inherit default :height 1.2))))
   
   ;; ------ gnus
   `(gnus-cite-face-1 ((t (:foreground "#ad7fa8"))))
   `(gnus-cite-face-2 ((t (:foreground "sienna4"))))
   `(gnus-cite-face-3 ((t (:foreground "khaki4"))))
   `(gnus-cite-face-4 ((t (:foreground "PaleTurquoise4"))))
   `(gnus-group-mail-1-empty-face ((t (:foreground "light cyan"))))
   `(gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))
   `(gnus-group-mail-2-empty-face ((t (:foreground "turquoise"))))
   `(gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))
   `(gnus-group-mail-3-empty-face ((t (:foreground "#729fcf"))))
   `(gnus-group-mail-3-face ((t (:bold t :foreground "#edd400"))))
   `(gnus-group-mail-low-empty-face ((t (:foreground "dodger blue"))))
   `(gnus-group-mail-low-face ((t (:bold t :foreground "dodger blue"))))
   `(gnus-group-news-1-empty-face ((t (:foreground "light cyan"))))
   `(gnus-group-news-1-face ((t (:bold t :foreground "light cyan"))))
   `(gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
   `(gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))
   `(gnus-group-news-3-empty-face ((t (:foreground "#729fcf"))))
   `(gnus-group-news-3-face ((t (:bold t :foreground "#edd400"))))
   `(gnus-group-news-low-empty-face ((t (:foreground "dodger blue"))))
   `(gnus-group-news-low-face ((t (:bold t :foreground "dodger blue"))))
   `(gnus-header-name-face ((t (:bold t :foreground "#729fcf"))))
   `(gnus-header-from ((t (:bold t :foreground "#edd400"))))
   `(gnus-header-subject ((t (:foreground "#edd400"))))
   `(gnus-header-content ((t (:italic t :foreground "#8ae234"))))
   `(gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
   `(gnus-signature-face ((t (:italic t :foreground "dark grey"))))
   `(gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
   `(gnus-summary-high-ancient-face ((t (:bold t :foreground "rotal blue"))))
   `(gnus-summary-high-read-face ((t (:bold t :foreground "lime green"))))
   `(gnus-summary-high-ticked-face ((t (:bold t :foreground "tomato"))))
   `(gnus-summary-high-unread-face ((t (:bold t :foreground "white"))))
   `(gnus-summary-low-ancient-face ((t (:italic t :foreground "lime green"))))
   `(gnus-summary-low-read-face ((t (:italic t :foreground "royal blue"))))
   `(gnus-summary-low-ticked-face ((t (:italic t :foreground "dark red"))))
   `(gnus-summary-low-unread-face ((t (:italic t :foreground "white"))))
   `(gnus-summary-normal-ancient-face ((t (:foreground "royal blue"))))
   `(gnus-summary-normal-read-face ((t (:foreground "lime green"))))
   `(gnus-summary-normal-ticked-face ((t (:foreground "indian red"))))
   `(gnus-summary-normal-unread-face ((t (:foreground "white"))))
   `(gnus-summary-selected ((t (:background "brown4" :foreground "white"))))
   ))

(provide-theme 'my-custom)
