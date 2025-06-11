;; list of configurations with no dependencies
;; load in front of init.el

(defvar ghost/debug-enable t)

(defun ghost/core-kbs ()
  (global-set-key (kbd "M-[") 'backward-paragraph) ;; delimited by newline
  (global-set-key (kbd "M-]") 'forward-paragraph)
  ;; (global-set-key (kbd "M-p") 'backward-sentence)
  ;; (global-set-key (kbd "M-n") 'forward-sentence)

  (keymap-global-set "C-;" 'comment-line)	;; C-x C-;
  (keymap-global-set "C-z" 'undo)		;; C-/
  ;; (keymap-global-set "C-." 'repeat)		;; C-x z
  (keymap-global-set "M-z" 'zap-up-to-char)	;; zap-to-char

  (global-set-key (kbd "C-c m") 'imenu)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; ------------
  ;; Cheatsheet
  ;; ------------
  ;; - "C-M-spc", mark-sexp
  ;;   mark sexp
  ;; - "M-s C-h"
  ;;   list bindings starts with "M-s" (Isearch)
  ;; - "M-j", default-indent-new-line
  ;;   insert newline with indentation
  ;; - "C-o", open-line
  ;;   insert newline, do not move point
  ;; - "C-j", electric-newline-and-maybe-indent,
  ;;   insert newline, maybe indent, move point

  ;; ------------
  ;; Windows
  ;; ------------
  (if (equal system-type 'windows-nt)
      (progn
	(global-set-key (kbd "s-p") 'previous-buffer)
	(global-set-key (kbd "s-n") 'next-buffer)))
  )


(defun ghost/core ()
  (ghost/core-kbs)

  ;; ------------
  ;; UI
  ;; ------------
  (setq inhibit-startup-message t)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (set-scroll-bar-mode nil)

  ;; Cheatsheet
  ;; ------------
  ;; cursor-type
  ;; set-scroll-bar-mode
  ;; (setq split-height-threshold nil)
  
  ;; Dired
  (setq dired-create-destination-dirs t)

  ;; UTF-8
  ;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
  (set-default-coding-systems 'utf-8)
    
  ;; Frame
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  ;; External display
  ;; (cond
  ;;  ((string-equal (getenv "EXT_DISPLAY") "t")
  ;;   (add-to-list default-frame-alist '(font . "someFont")))
  ;;  (t))

  ;; Buffer
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode)
  (electric-pair-mode)

  ;; Minibuffer
  (savehist-mode)
  (setq
   completion-ignore-case t
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t)
  (setq tab-always-indent 'complete)
  (setq enable-recursive-minibuffers t)
  ;; keep cursor out of prompt
  (setq minibuffer-prompt-properties
        '(read-only t face minibuffer-prompt cursor-intangible t))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Tuning LSP perf
  (setq gc-cons-threshold (* 1024 1024 2))	;; 2mb
  (setq read-process-output-max (* 1024 1024))	;; 1mb
  
  ;; ------------
  ;; Other
  ;; ------------
  (setq debug-on-error ghost/debug-enable)
  (setq use-package-expand-minimally t)
  (setq ring-bell-function 'ignore) 	;; ignore bells
  ;; (setq visible-bell t)

  ;; WSL
  ;; ------------
  ;; https://hungyi.net/posts/browse-emacs-urls-wsl/
  ;; (if (and (equal system-type 'gnu/linux)
  ;; 	   (string-match "microsoft"
  ;; 			 (shell-command-to-string "uname -a")))
  ;;     (setq
  ;;      browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
  ;;      browse-url-generic-args '("/c" "start")
  ;;      browse-url-browser-function #'browse-url-generic))
  
  )

(provide 'ghost-core)
