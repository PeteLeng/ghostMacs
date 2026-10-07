;;; init.el --- Minimal Emacs 31 configuration -*- lexical-binding: t; -*-

;; ============================================================================
;;; Local files
;; ============================================================================

(defgroup lean nil
  "Local Emacs profile."
  :group 'environment)

(defcustom lean/local-directory (expand-file-name "local/" user-emacs-directory)
  "Directory for local history, backups, and other generated files.
Set before the paths below are initialized."
  :type 'directory
  :group 'lean)

(defun lean/local-path (name)
  "Expand NAME relative to `lean/local-directory'."
  (expand-file-name name lean/local-directory))

(make-directory lean/local-directory t)
(make-directory (lean/local-path "backups/") t)
(make-directory (lean/local-path "auto-save/") t)

;; Customize saves here, but saved settings are not restored on startup.
(setopt custom-file (lean/local-path "custom.el"))

(setopt backup-directory-alist `(("." . ,(lean/local-path "backups/")))
        auto-save-file-name-transforms `((".*" ,(lean/local-path "auto-save/") t))
        auto-save-list-file-prefix (lean/local-path "auto-save/.saves-"))

;; ============================================================================
;;; UI
;; ============================================================================

(setopt inhibit-startup-screen t
        ring-bell-function #'ignore) ; Silence the bell.

(load-theme 'wombat t)

;; ============================================================================
;;; Editing
;; ============================================================================

(keymap-global-set "M-[" #'backward-paragraph)
(keymap-global-set "M-]" #'forward-paragraph)
(keymap-global-set "C-;" #'comment-line)
(keymap-global-set "C-z" #'undo)
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "C-c m" #'imenu)
(keymap-global-set "C-x C-b" #'ibuffer)

;; Load the option definition before setopt checks its type.
(require 'display-line-numbers)
(setopt display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(electric-pair-mode 1)

;; TAB indents first, then tries completion when indentation is already correct.
(setopt tab-always-indent 'complete)

;; ============================================================================
;;; Windows
;; ============================================================================

(defun lean/grow-window (direction amount)
  "Grow the selected split toward DIRECTION by AMOUNT lines or columns.
DIRECTION is left, right, up, or down.  Respect window size limits."
  (let* ((horizontal (memq direction '(left right)))
         (backward (memq direction '(left up)))
         (window (selected-window))
         sibling)
    (when (window-size-fixed-p window horizontal)
      (user-error "Selected window has a fixed size"))
    ;; Find the nearest shared edge, including in nested split layouts.
    (while (and window
                (not (and (window-combined-p window horizontal)
                          (setq sibling
                                (if backward
                                    (window-prev-sibling window)
                                  (window-next-sibling window))))))
      (setq window (window-parent window)))
    (unless window
      (user-error "No neighboring split toward %s" direction))
    ;; Left/up move the preceding split's trailing edge backwards.
    (adjust-window-trailing-edge (if backward sibling window)
                                 (if backward (- amount) amount)
                                 horizontal)))

;; Each arrow grows toward its neighbor; a prefix sets the resize amount.
(keymap-global-set "C-M-<left>"
                   (lambda (amount) (interactive "p")
                     (lean/grow-window 'left amount)))
(keymap-global-set "C-M-<right>"
                   (lambda (amount) (interactive "p")
                     (lean/grow-window 'right amount)))
(keymap-global-set "C-M-<up>"
                   (lambda (amount) (interactive "p")
                     (lean/grow-window 'up amount)))
(keymap-global-set "C-M-<down>"
                   (lambda (amount) (interactive "p")
                     (lean/grow-window 'down amount)))

;; ============================================================================
;;; Minibuffer
;; ============================================================================

(setq completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        enable-recursive-minibuffers t)

(require 'savehist)
(setopt savehist-file (lean/local-path "history"))
(savehist-mode 1)

;; Mark prompt text as cursor-intangible.  The option setter also enables
;; cursor-intangible-mode in each minibuffer so movement honors that property.
(setopt minibuffer-prompt-properties
        (plist-put minibuffer-prompt-properties 'cursor-intangible t))

;; ============================================================================
;;; Completion
;; ============================================================================

;; Flex matches characters in order with gaps: "ff" can match "find-file".
;; File paths prefer prefix and path-component completion.
(setopt completion-styles '(flex basic)
        completion-category-overrides '((file (styles basic partial-completion))))

;; Height includes the prompt, leaving roughly seven lines for candidates.
(setopt max-mini-window-height 8
        resize-mini-windows 'grow-only)

;; M-j returns prompt text as-is; <RET> accepts the selected completion candidate.
(fido-vertical-mode 1)
;; The graphical Tab key inserts the candidate without leaving the prompt.
(keymap-set icomplete-fido-mode-map "<tab>" #'icomplete-force-complete)

;; Stock *Completions* fallback, bypassed after <tab> override; keep anyway
(setopt completion-auto-help nil)

;; ============================================================================
;;; Straight
;; ============================================================================

(defun lean/git-config-set-kv (key value)
  "Set runtime Git KEY to VALUE for this Emacs session's subprocesses.
KEY and VALUE are strings.  Preserve other runtime entries and return VALUE."
  (let* ((count (string-to-number
                 (or (getenv "GIT_CONFIG_COUNT") "0")))
         (index count))
    ;; Update the last match; otherwise INDEX is the append position.
    (dotimes (j count)
      (when (equal (getenv (format "GIT_CONFIG_KEY_%d" j)) key)
        (setq index j)))
    (setenv (format "GIT_CONFIG_KEY_%d" index) key)
    (setenv (format "GIT_CONFIG_VALUE_%d" index) value)
    (when (= index count)
      (setenv "GIT_CONFIG_COUNT" (number-to-string (1+ count))))
    value))

;; OpenSSL completed our GitHub clone tests where Schannel stalled.
;; The underlying cause remains unconfirmed; use a session-local workaround.
(when (eq system-type 'windows-nt)
  (lean/git-config-set-kv "http.https://github.com/.sslBackend" "openssl"))

;; Bootstrap straight on first startup; later starts load its local checkout.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://radian-software.github.io/straight.el/install.el"
         'silent 'inhibit-cookies)
      ;; Straight's installer supplies its bootstrap code as the final expression.
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ============================================================================
;;; In-buffer completion
;; ============================================================================

(straight-use-package 'company)
(require 'company)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-require-match nil ; Allow typing outside the candidate list.
      company-dabbrev-ignore-case 'keep-prefix
      company-dabbrev-downcase nil)
;; company-global-modes controls eligible major modes; its default t means all.
(global-company-mode 1)

;; ============================================================================
;;; Org
;; ============================================================================

;; Register bundled Org before org-appear; :inherit nil skips recipe catalogs.
(straight-use-package '(org :type built-in :inherit nil))

(with-eval-after-load 'org
  (setopt org-cycle-separator-lines -1 ; Preserve blank lines in folded views.
          org-hide-emphasis-markers t
          org-hide-leading-stars t
          org-image-actual-width nil ; Respect per-image width attributes.
          org-pretty-entities t
          org-use-speed-commands t)) ; Single-key commands at heading starts.

(add-hook 'org-mode-hook #'org-indent-mode)

;; Hidden markup becomes visible while editing it.
(straight-use-package 'org-appear)
(with-eval-after-load 'org-appear
  (setopt org-appear-autosubmarkers t
          org-appear-autoentities t
          org-appear-inside-latex t))
(add-hook 'org-mode-hook #'org-appear-mode)

;; ============================================================================
;;; Navigation
;; ============================================================================

(straight-use-package 'avy)
(keymap-global-set "M-g l" #'avy-goto-line)

(straight-use-package 'ace-window)
(with-eval-after-load 'ace-window
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
          aw-background nil
          aw-dispatch-always t))
(keymap-global-set "M-o" #'ace-window)

(straight-use-package 'expand-region)
(require 'expand-region)
(keymap-global-set "C-/" #'er/expand-region)

;;; init.el ends here
