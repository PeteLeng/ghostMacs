;; -*- lexical-binding: t -*-

;; test global variables
;; (defvar mech-global -99)
;; (defvar mech-global 100)
;; (defvar vglob -99)


;; ------------
;; Libs
;; ------------
;; (straight-use-package 's)		;; list lib
;; (straight-use-package 'dash) 		;; string lib
;; (straight-use-package 'aio)
;; (straight-use-package 'graphql-mode)

;; Leetcode
;; (straight-use-package 'leetcode)
;; Not working and havn't got time to dig into it.


;; ------------
;; Canvas
;; ------------
(defmacro ghost/advice-once! (sym adv where)
  `(advice-add ',sym ,where
	       #'(lambda (orig &rest args)
		   (apply ',adv orig args)
		   (advice-remove ',sym #',adv))))

(defun ghost/inspect(orig &rest args)
  (with-current-buffer "*scratch*"
    (goto-char (point-max))
    (insert (format "%s\n\n" args))
    (insert (format "%s\n\n" (car args))))
  (apply orig args))

(defun ghost/list-fonts ()
  (with-current-buffer "*scratch*"
    ;; (goto-char (point-max))
    (dolist (f (font-family-list))
      (insert (format "%s\n" f))
      )))

;; (ghost/advice-once! lsp-register-client ghost/inspect :around)
;; (message "%s" (symbol-plist 'lsp-register-client))

;; (defun org-ast (fpath)
;;   (let ((tree
;; 	 (with-temp-buffer
;; 	   (insert-file-contents fpath)
;; 	   (org-mode)
;; 	   (org-element-parse-buffer))))
;;     tree))

;; (defun org-headline-path (tree)
;;   (org-element-map tree 'headline
;;     #'(lambda (hl)
;; 	(let ((path ())
;; 	      (hl hl))
;; 	  (while (not (equal (org-element-property :raw-value hl) nil))
;; 	    (push (org-element-property :raw-value hl) path)
;; 	    (setq hl (org-element-property :parent hl)))
;; 	  (string-join path "> "))))
;;   )

;; (defun org-title-name (tree)
;;   (org-element-map tree 'keyword
;;     #'(lambda (kw)
;; 	(if (equal (org-element-property :key kw) "TITLE")
;; 	    (org-element-property :value kw)))
;;     nil t)) ;; Stop at first match

;; (defun roam-proj-files (proj-dir)
;;   (file-expand-wildcards (file-truename (file-name-concat proj-dir "*.org")))
;;   )

;; (defun roam-proj-headlines (proj-dir)
;;   (let* ((files
;; 	  (roam-proj-files proj-dir))
;; 	 (headlines ()))
;;     (dolist (file files)
;;       (let* ((tree
;; 	      (org-ast file))
;; 	     (title
;; 	      (org-title-name tree))
;; 	     (hls
;; 	      (org-headline-path tree)))
;; 	;; (setq headlines (append headlines hls))
;; 	(dolist (hl hls)
;; 	  (push (concat title "> " hl) headlines))))
;;     headlines))

;; (defun mech-capture ()
;;   (interactive)
;;   (let* ((title
;; 	  (read-string "Title:"))
;; 	 (target
;; 	  (completing-read "Target" (list "a" "b" "c"))))
;;     (message "%s" title)
;;     (message "%s" target)
;;     )
;;   )

;; (defun mech-get-fonts (regex)
;;   (insert "\n")
;;   (dolist (f (font-family-list))
;;     (when (string-match regex f)
;;       (insert (format "%s\n" f)))))


;; ------------
;; Depr
;; ------------
;; straight.el projectile fix
;; ;; fix unwanted warning in Projectile
;; (defun straight-files-exl-gignore ()
;;   (let ((files '()))
;;     (dolist (p straight-default-files-directive)
;;       (if (stringp p)
;; 	  (add-to-list 'files p t)
;; 	(let ((exl p))
;; 	  (setq exl (add-to-list 'p ".gitignore" t))
;; 	  (add-to-list 'files exl t))))
;;     files))

;; (defun mech-expand-files-log (fapp &rest args)
;;   (let* ((res (apply fapp args)))
;;     (message "Sym links:")
;;     ;; (message "%s" res)
;;     (dolist (edge res)
;;       (let ((src (car edge))
;; 	    (dst (cdr edge)))
;; 	(message "%s -> %s" src dst)))
;;     res))
;; ;; (advice-add 'straight-expand-files-directive :around #'mech-expand-files-log)
;; ;; (advice-remove 'straight-expand-files-directive #'mech-expand-files-log)

;; (defun mech-symlink-advice (fapp &rest args)
;;   (let ((src (nth 0 args))
;; 	(dst (nth 1 args)))
;;     (if (string-equal (file-name-base src) ".gitignore")
;; 	(message "ignore .gitignore as symlink")
;;       (apply fapp args))))
;; (advice-add 'straight--symlink-recursively :around #'mech-symlink-advice)

;; tree-sitter
;; ------------
;; this library should no longer be used in emacs 30
;; as it requires external packages 'elisp-tree-sitter',
;; which is already built-in
;; (straight-use-package 'tree-sitter-langs)

;; install grammers (copy to user-emacs-directory/tree-sitter/)
;; ------------
;; (defun ghost/ts-install-grammer (orig-fun &rest args)
;;   (apply orig-fun args)
;;   (let* ((src-dir (tree-sitter-langs--bin-dir))
;; 	 (grammer-files (directory-files src-dir t "\\.so$"))
;; 	 (dst-dir (concat user-emacs-directory "tree-sitter/"))
;; 	 (_ (unless (file-directory-p dst-dir)
;; 	      (make-directory dst-dir))))
;;     (dolist (f grammer-files)
;;       (make-symbolic-link f (concat dst-dir "libtree-sitter-" (file-name-nondirectory f)) t))))

;; (unless (boundp 'tree-sitter-langs-install-grammars)
;;   (autoload 'tree-sitter-langs-install-grammers
;;     "tree-sitter-langs-build.el" t t))
;; (advice-add 'tree-sitter-langs-install-grammars
;; 	    :around #'ghost/ts-install-grammer)

;; (require 'tree-sitter-langs)
