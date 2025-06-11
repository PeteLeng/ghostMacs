;; ------------
;; Buffer Movements
;; ------------
(straight-use-package 'ace-window)

(with-eval-after-load 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  ;; https://oremacs.com/2015/02/27/ace-window-leading-char/
  ;; (custom-set-faces
  ;;  '(aw-leading-char-face
  ;;    ((t (:inherit ace-jump-face-foreground :height 1.1)))))
  )

(unless (fboundp 'ace-window)
  (autoload #'ace-window "ace-window" nil t))
(keymap-global-set "M-o" 'ace-window)

;; ------------
;; Mark Text (Selection)
;; ------------
(straight-use-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-/") 'er/expand-region)

;; ------------
;; Multiple Cursors
;; ------------
;; https://github.com/magnars/multiple-cursors.el
(straight-use-package 'multiple-cursors)
(keymap-global-set "C-M-s-," 'mc/mark-previous-like-this)
(keymap-global-set "C-M-s-." 'mc/mark-next-like-this)
(keymap-global-set "C-M-s-/" 'mc/mark-all-like-this)
(keymap-global-set "C-M-s-c c" 'mc/edit-lines)
(keymap-global-set "M-<mouse-8>" 'mc/add-cursor-on-click) ;; got extra thumb buttons

;; ------------
;; Templates
;; ------------
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode)

(straight-use-package 'yasnippet-snippets)
(require 'yasnippet-snippets)

;; ------------
;; Navigation
;; ------------
(keymap-global-set "C-M-s-l" 'avy-goto-line)
(keymap-global-set "C-M-s-w" 'avy-goto-word-1)
;; (keymap-global-set "M-g oh" 'avy-org-goto-heading-timer)

;; ------------
;; Folding
;; ------------
(with-eval-after-load "hideshow"
  ;; keybinding choice in hideshow is bad, occluding C-h utility
  ;; disable @
  ;; https://emacs.stackexchange.com/a/33686
  ;; (keymap-set hs-minor-mode-map "C-c @" nil)
  (keymap-unset hs-minor-mode-map "C-c @")
  (define-key hs-minor-mode-map (kbd "C-c h h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h s") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h H") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c h S") 'hs-show-all)
  (keymap-set hs-minor-mode-map "C-c h l" 'hs-hide-level)
  (keymap-set hs-minor-mode-map "C-c h t" 'hs-toggle-hiding))
(add-hook 'prog-mode-hook #'hs-minor-mode)

