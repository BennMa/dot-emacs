(use-package highlight-parentheses
  :diminish (highlight-parentheses-mode . "")
  :config
  (global-highlight-parentheses-mode))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package indent-guide
  :config
  ;; (set-face-background 'indent-guide-face "dimgray")
  (indent-guide-global-mode))

(use-package hl-line
  :diminish (hl-line-mode . "")
  :commands hl-line-mode
  :bind (("M-o h" . hl-line-mode))
  :config
  ;; (use-package hl-line+)
  ;; (global-hl-line-mode)
  )

(use-package maxframe
  :if window-system
  :commands maximize-frame 
  :bind (("C-c M" . emacs-max)
         ("C-c m" . emacs-toggle-size))
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

(use-package popwin :config (popwin-mode 1))
;; Enforce rules for popup windows
;; https://github.com/wasamasa/shackle
(use-package shackle :disabled t :config (shackle-mode))
