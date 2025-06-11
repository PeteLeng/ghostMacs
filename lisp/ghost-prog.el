;; -*- lexical-binding: t -*-

(defvar ghost/lsp-client "lsp")
;; lsp client comparisons
;; https://www.reddit.com/r/emacs/comments/1c0v28k/lspmode_vs_lspbridge_vs_lspce_vs_eglot/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

;; ------------
;; Treesitter
;; ------------

;; Install Grammers (copy to user-emacs-directory/tree-sitter/)
;; ------------
;; for now I use the bash script from casouri's repo
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; should move to a more lisp approach in the future
;; https://leba.dev/blog/2022/12/12/(ab)using-straightel-for-easy-tree-sitter-grammar-installations/

(treesit-language-available-p 'python)	; test installation 

(defvar ghost/remap-major-mode-langs
  '("c" "c++" "python" "rust" "java" "bash" "yaml" "toml" "json" "css"))
(dolist (lang ghost/remap-major-mode-langs)
  (add-to-list 'major-mode-remap-alist
	       (cons (intern (concat lang "-mode"))
		     (intern (concat lang "-ts-mode")))))


;; ------------
;; Git
;; ------------
(straight-use-package 'magit)

;; ------------
;; Keybindings
;; ------------
(keymap-global-set "<f6>" 'recompile)


;; ------------
;; Compilation
;; ------------
(setq compilation-scroll-output t)
(setq compilation-auto-jump-to-first-error t)

;; ------------
;; Language Configs
;; ------------

;; Python
;; ------------
(straight-use-package 'pyvenv)
;; (add-hook 'python-mode-hook #'pyvenv-mode)

;; CUDA
;; ------------
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))


;; ------------
;; Language Server
;; ------------
(when (string-equal ghost/lsp-client "lsp")
  (straight-use-package 'lsp-mode)
  ;; (require 'lsp-mode)
  (setq
   lsp-idle-delay 0.5
   lsp-keep-workspace-alive nil
   lsp-keymap-prefix "C-c l"			
   lsp-log-io nil			; t for debug only
   lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory)

   ;; ported from
   ;; https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
   lsp-auto-configure t
   lsp-eldoc-enable-hover nil
   lsp-enable-dap-auto-configure nil
   lsp-enable-file-watchers nil
   lsp-enable-folding nil
   lsp-enable-imenu t
   lsp-enable-indentation nil
   lsp-enable-links nil
   lsp-enable-on-type-formatting nil
   lsp-enable-suggest-server-download nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-text-document-color nil
   lsp-enable-xref t
   lsp-signature-doc-lines 5

   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-enable-diagnostics nil
   lsp-headerline-breadcrumb-enable-symbol-numbers nil
   lsp-headerline-breadcrumb-icons-enable nil

   lsp-modeline-code-actions-enable t
   lsp-modeline-diagnostics-enable t
   lsp-modeline-workspace-status-enable nil

   ;; other
   ;; lsp-use-workspace-root-for-server-default-directory t
   )

  (defvar ghost/lsp-autostart-langs
    '("c" "c++" "python" "rust" "java" "sh"))
  (unless (fboundp 'lsp-deferred)
    (autoload #'lsp-deferred "lsp-mode" nil nil))
  ;; incorporate ts modes
  (dolist (lang ghost/lsp-autostart-langs)
    (add-hook (intern (string-join `(,lang "-ts-mode-hook"))) #'lsp-deferred))


  ;; Sub-modules
  ;; ------------
  
  (defvar ghost/lang-client-alist
    '((python . lsp-pyright)
      (java . lsp-java)))

  (defvar ghost/lang-hook-alist
    '((python . python-ts-mode-hook)
      (java . java-ts-mode-hook)))

  ;; macro do not evaluate their input
  ;; cannot figure out how to create dynamically named functions
  ;; (defmacro ghost/lang-hook-f (lang client &rest body)
  ;;   (let ((func (intern (concat "ghost/"
  ;; 				(symbol-name lang) "-confs"))))
  ;;     `(defun ,func ()
  ;; 	 (require ',client)
  ;; 	 ,@body)))

  (defun ghost/lang-hook-f (lang client &rest body)
    (let* ((func (intern (concat "ghost/"
				 (symbol-name lang) "-confs")))
	   (decl `(defun ,func () (require ',client) ,@body) ))
      (eval decl)			; use fset to avoid eval
      func))

  (dolist (ele ghost/lang-client-alist)
    (pcase ele
      (`(,lang . ,client)
       (let ((hook (alist-get lang ghost/lang-hook-alist))
	     (func (ghost/lang-hook-f lang client)))
	 (message "installing %s: %s" hook func)
	 (add-hook hook func)))
      (_ (error "malformed lang client pair: %s" ele))))


  
  ;; Python
  (straight-use-package 'lsp-pyright)
  ;; https://github.com/emacs-lsp/lsp-pyright/issues/6
  (setq lsp-pyright-multi-root nil)
  ;; (add-hook 'python-ts-mode-hook #'(lambda ()
  ;; 				     (require 'lsp-pyright)))

  ;; Java
  ;; https://emacs-lsp.github.io/lsp-java/
  (straight-use-package 'lsp-java)
  (setq lsp-java-server-install-dir "/home/pete/opt/java-lsp/")
  ;; (setq lsp-java-format-settings-url "https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings")
  ;; (add-hook 'java-mode-hook #'(lambda ()
  ;; 				(require 'lsp-java)))
  
  )

;;; ghost-prog.el ends here
