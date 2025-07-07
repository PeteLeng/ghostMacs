(defvar ghost/completion-frmwk "company")

;; ------------
;; Orderless
;; ------------
(straight-use-package 'orderless)
(require 'orderless)
(setq completion-styles '(basic orderless))

;; ------------
;; Vertico
;; ------------
(straight-use-package 'vertico)
(require 'vertico)
(vertico-mode)
(setq vertico-count 7)
(setq vertico-resize nil)

;; ------------
;; Company
;; ------------
(when (equal ghost/completion-frmwk "company")
  (straight-use-package 'company)
  (require 'company)
  ;; https://www.reddit.com/r/emacs/comments/m52nky/comment/gr2ap03/?utm_source=share&utm_medium=web2x&context=3
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.1
   company-tooltip-limit 10
   company-tooltip-align-annotations t		;; Align annotations to the right
   company-require-match nil			;; Allow free typing
   company-dabbrev-ignore-case 'keep-prefix	;; Keep prefix at completion
   company-dabbrev-downcase nil			;; Don't automatically downcase completions
   )
  ;; https://stackoverflow.com/a/11573802/17006775
  ;; (setq company-dabbrev-other-buffers t)
  ;; (setq company-dabbrev-ignore-buffers ".*\\.pdf")
  
  (setq company-global-modes '(not shell-mode eshell-mode))
  ;; (delete 'company-clang company-backends)
  
  (global-company-mode))

;; ------------
;; Corfu
;; ------------
;; slow from previous experiences
(when (eq ghost/completion-frmwk "corfu")
  (straight-use-package 'corfu)
  (global-corfu-mode)
  (setq
   corfu-auto t
   corfu-auto-delay 0.4 ;; lower delay hangs?
   corfu-quit-no-match 'separator))


;; ------------
;; Other
;; ------------
;; ;; speed up completion in WSL
;; ;; exclude host (win) path during completion
;; ;; lol this is a poorly written function
;; (defun wsl-completion-tbl-gen-excl (orig &rest args)
;;   (let ((prev-exec-path exec-path))
;;     (dolist (path prev-exec-path)
;;       (if (booleanp (compare-strings "/mnt/c" nil nil path nil 6))
;;           (setq exec-path (remove path exec-path))))
;;     ;; (dolist (path exec-path)
;;     ;;   (insert (concat path "\n")))
;;     (apply orig args)
;;     ))

;; (unless (fboundp 'sh--cmd-completion-table-gen)
;;   (autoload #'sh--cmd-completion-table-gen "sh-script" nil nil))
;; (advice-add 'sh--cmd-completion-table-gen
;; 	    :around #'wsl-completion-tbl-gen-excl)

;;; ghost-complete.el ends here
