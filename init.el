(add-to-list 'load-path
	     (expand-file-name "lisp/" user-emacs-directory))
(require 'ghost-core)
(ghost/core)
(load-theme 'modus-vivendi)

;; Straight
;; ------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; in case raw URL blocked, use "https://radian-software.github.io/straight.el/install.el"

;; <25/05/03> had to clone nongnu manually, straight clone just won't work
;; git clone --origin origin --no-checkout https\://git.savannah.gnu.org/git/emacs/nongnu.git /home/pete/.emacs-profs/foo/straight/repos/nongnu-elpa/ --single-branch

(when (string-equal (daemonp) "gh")
  ;; daemon started by systemd, with minimal environment
  (load "ghost-daemon"))

(load "ghost-util")
(setq ghost/completion-frmwk "company")
(load "ghost-complete")
(load "ghost-edit")
(setq ghost/lsp-client "lsp")
(load "ghost-prog")
(load "ghost-note")

;; Projectile, Magit
;; (load "mech-project")

;; Org, Org-roam
;; (load "mech-org")
;; (load "mech-roam")

;; Utilities
;; (load "mech-util")

;; init.el ends here.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((elisp-lint-indent-specs (describe . 1) (it . 1) (thread-first . 0)
			      (cl-flet . 1) (cl-flet* . 1)
			      (org-element-map . defun)
			      (org-roam-dolist-with-progress . 2)
			      (org-roam-with-temp-buffer . 1)
			      (org-with-point-at . 1)
			      (magit-insert-section . defun)
			      (magit-section-case . 0)
			      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
