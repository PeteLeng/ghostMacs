;;; early-init.el --- Startup settings -*- lexical-binding: t; -*-

;; ============================================================================
;;; Executable search paths
;; ============================================================================

(require 'subr-x)

(defun lean/normalize-directory (dir)
  "Normalize Windows directory spelling for comparison."
  (thread-last dir
    (subst-char-in-string ?\\ ?/)
    directory-file-name
    downcase))

(when (eq system-type 'windows-nt)
  (let* ((msys2-dirs '("C:/dev/msys64/ucrt64/bin"
                      "C:/dev/msys64/usr/bin"))
         (normalized-dirs (mapcar #'lean/normalize-directory msys2-dirs))
         ;; Match alternate spellings without rewriting unrelated entries.
         (msys2-dir-p
          (lambda (dir)
            (and (stringp dir)
                 (let ((normalized-dir (lean/normalize-directory dir)))
                   (seq-some
                    (lambda (preferred-dir)
                      (string-equal normalized-dir preferred-dir))
                    normalized-dirs)))))
         (path-dirs (split-string (or (getenv "PATH") "")
                                  path-separator nil)))
    (setopt exec-path
            (append msys2-dirs (seq-remove msys2-dir-p exec-path)))
    ;; Preserve PATH's own entries, including any trailing empty component.
    (setenv "PATH"
            (string-join
             (append msys2-dirs (seq-remove msys2-dir-p path-dirs))
             path-separator))))

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
