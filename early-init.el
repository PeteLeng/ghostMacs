;;; early-init.el --- Startup settings -*- lexical-binding: t; -*-

;; ============================================================================
;;; Package activation
;; ============================================================================

;; Set before package.el loads; straight handles activation instead.
(setq package-enable-at-startup nil)

;; ============================================================================
;;; UI
;; ============================================================================

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; These defaults apply to the first graphical frame as well as later frames.
(when (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font . "Consolas-14")))

;; Background-only opacity; text remains opaque.
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;; early-init.el ends here
