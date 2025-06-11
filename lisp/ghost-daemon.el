(straight-use-package
 '(exec-path-from-shell :type git
			:host github
			:repo "purcell/exec-path-from-shell"))

(require 'exec-path-from-shell)
(defvar gh/envars '(
		    ;; "LANG"
		    ;; "SSH_AUTH_SOCK"
		    "SSH_AGENT_PID"
		    "GPG_AGENT_INFO"
		    "LSP_USE_PLISTS"))
(dolist (var gh/envars)
  (add-to-list 'exec-path-from-shell-variables var))
;; (exec-path-from-shell-initialize)
