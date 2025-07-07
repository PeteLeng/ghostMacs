
;; ------------
;; Blog
;; ------------
;; (straight-use-package 'ox-hugo)


;; ------------
;; Org
;; ------------
(straight-use-package '(org :type built-in)) ; stop straight from cloning org

;; Cosmetics
;; ------------
(straight-use-package 'org-appear)
(with-eval-after-load 'org-appear
  (setq org-appear-autosubmarkers t
	org-appear-autoentities t
	org-appear-inside-latex t))

(defun ghost/org-prettify ()
  (setq prettify-symbols-unprettify-at-point
	'right-edge) ; set before mode
  (defvar ghost/prettify-symbols-alist
    '(("#+END_SRC" . "―")
      ("#+end_src" . "―")
      (":PROPERTIES:" . "")
      (":END:" . "―")
      (":Effort:" . "")
      ("SCHEDULED:" . "")
      ("lambda" . "λ")
      ("|>" . "▷")
      ("<|" . "◁")
      ("->>" . "↠")
      ("->" . "→")
      ("<-" . "←")
      ("=>" . "⇒")
      ("<=" . "≤")
      (">=" . "≥")
      ("!=" . "≠")))
  (dolist (ele ghost/prettify-symbols-alist)
    ;; new kv pair will shadow previous vals in alist 
    (add-to-list 'prettify-symbols-alist ele))
  (prettify-symbols-mode))


;; Navigation
;; ------------
(defun ghost/org-goto-heading-bf ()
  "Move cursor to its surrounding Org heading,
the preceding heading takes precedence."
  (interactive)
  ;; consider using org-get-limited-outline-regexp instead.
  (let ((st (point))
	(rgx (concat "^" org-outline-regexp)))
    (end-of-line)
    (cond
     ((re-search-backward rgx nil t))
     ;; - if no match, do a forward search
     ((re-search-forward rgx nil t) (forward-line 0))
     (t (goto-char st)))))


;; Global Org Keybindings
;; ------------
(defmacro ghost/autoload! (func file &rest args)
  `(unless (fboundp ',func)
     (autoload #',func ,file ,@args)))

(ghost/autoload! org-store-link "org" nil t)
(ghost/autoload! org-capture "org" nil t)
(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c c" 'org-capture)

;; Configs
;; ------------
(setq
 org-cycle-separator-lines -1	; show all empty lines when collapsed
 org-hide-emphasis-markers t
 org-hide-leading-stars t
 org-image-actual-width nil ;	; inline image resize
 org-pretty-entities t		; show \symb as UTF8
 org-use-speed-commands t
 
 ;; org-cycle-include-plain-lists 'integrate ; tab cycle view
 ;; org-fontify-quote-and-verse-blocks t
 ;; org-startup-folded 'nofold
 )

;; Hook
;; ------------
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(when org-hide-emphasis-markers
  (add-hook 'org-mode-hook #'org-appear-mode))
(add-hook 'org-mode-hook #'ghost/org-prettify)


(with-eval-after-load 'org
  ;; export
  ;; ------------
  (setq org-cite-global-bibliography '("~/org/bib/cite.bib"))
  (setq org-cite-export-processors
	'((latex . (biblatex "authoryear-icomp"))
	  (t basic)))
  ;; https://emacs.stackexchange.com/a/60727
  (setq org-latex-pdf-process
	'("pdflatex -interaction nonstopmode -output-directory %o %f"
          "biber --output-directory %o $(basename %f .tex)"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  
  ;; http link face
  ;; ------------
  (defface ghost/org-http-link
    '((t (:inherit org-link :slant italic))) "Org Http link")
  ;; (org-link-set-parameters "http" :face 'ghost/org-http-link)
  ;; (org-link-set-parameters "https" :face 'ghost/org-http-link)

  ;; Latex
  ;; ------------
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 2))
  (setq org-latex-src-block-backend 'listings)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))

  ;; Org mode keybindings
  (pcase system-type
    ('gnu/linux
     (keymap-set org-mode-map "C-M-s-h" 'ghost/org-goto-heading-bf))
    ('windows-nt
     (keymap-set org-mode-map "M-g h" 'ghost/org-goto-heading-bf)))
  
  )


;; ------------
;; Roam
;; ------------

(straight-use-package 'org-roam)
(defvar ghost/roam-dir (file-truename "d:/yleng/org/hw/"))
(defvar ghost/roam-tmpl-dir (file-truename "d:/yleng/org/tmpls/"))
(setq org-roam-directory ghost/roam-dir)

;; Global Roam Keybindings
;; ------------
(ghost/autoload! org-roam-dailies-map "org-roam" t t)
(ghost/autoload! org-roam-node-find "org-roam" t t)
(ghost/autoload! org-roam-node-insert "org-roam" t t)
(ghost/autoload! org-roam-node-random "org-roam" t t)

(keymap-global-set "C-c n j" 'org-roam-dailies-map)
(keymap-global-set "C-c n f" 'org-roam-node-find)
(keymap-global-set "C-c n i" 'org-roam-node-insert)
(keymap-global-set "C-c n r" 'org-roam-node-random)

;; Custom Utils
;; ------------
(defun ghost/roam-cleanup ()
  "Move least recently modified files from canvas to drafts."
  (let* ((src-dir "~/org/vault/canvas/")
	 (dst-dir "~/org/vault/drafts/")
	 (day-secs (* 24 60 60))
	 (curr-time (time-convert (current-time) 'integer))
	 (rgx "^.*[^.]$")
	 (alist (directory-files-and-attributes src-dir t rgx)))
    (dolist (ele alist)
      (let* ((fname (car ele))
	     (attrs (cdr ele))
	     (mtime (time-convert (file-attribute-modification-time attrs)
				  'integer))
	     (dur-secs (- curr-time mtime)))
	(message (format "%s last modified: %s days ago"
			 (file-name-nondirectory fname)
			 (/ dur-secs day-secs)))
	(when (> (/ dur-secs day-secs) 15)
	  (rename-file fname dst-dir))))))
(add-hook 'org-roam-mode-hook #'ghost/roam-cleanup)

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode)
  
  ;; Roam Keybindings
  ;; ------------
  (keymap-set org-mode-map "C-c n a" 'org-roam-alias-add)
  (keymap-set org-mode-map "C-c n b" 'org-roam-buffer-toggle)
  (keymap-set org-mode-map "C-c n k" 'org-id-get-create)
  (keymap-set org-mode-map "C-c n t" 'org-roam-tag-add)

  ;; Never got to figure out how to use refile lol

  ;; Mini-buffer Display Template For Nodes
  ;; ------------
  ;; Roam internally uses org-capture to create node
  ;; which uses 'buffer-file-name' to capture file name
  ;; which returns absolute path
  ;; org-roam-node is a struct
  ;; ((node org-roam-node)) is the argument list
  ;; each argument is of the form (arg spec)
  ;; where arg is the name, and spec is the type etc
  (cl-defmethod org-roam-node-subdir ((node org-roam-node))
    "Return subdir of the file within roam root.
Empty if directly under root."
    (let* ((f (org-roam-node-file node))
	   (dir (file-name-directory f)))
      (message (string-remove-prefix org-roam-directory f))
      (string-remove-prefix org-roam-directory dir)))
  
  (setq org-roam-node-display-template
	(concat "${title:60} "
		"- "
		(propertize "${subdir:10}" 'face 'org-tag)
		(propertize "${tags:20}" 'face 'org-tag)))


  ;; Roam Templates
  ;; ------------
  (defmacro ghost/get-tmpl (tmpl)
    `(concat ,ghost/roam-tmpl-dir ,tmpl ".org"))
  ;; (macroexpand '(ghost/get-tmpl "default"))
  
  (defvar ghost/roam-tmpls
    `(("d" "default" plain (file ,(ghost/get-tmpl "default"))
       :target (file "canvas/%<%Y%m%d%H%M%S>-${slug}.org"))
      ("l" "latex" plain (file ,(ghost/get-tmpl "latex"))
       :target (file "canvas/%<%Y%m%d%H%M%S>-${slug}.org"))
      ("b" "blogs" plain (file ,(ghost/get-tmpl "blog"))
       :target (file "blogs/%<%Y%m%d%H%M%S>-${slug}.org"))))
  
  (setq org-roam-capture-templates ghost/roam-tmpls)

  ;; Daily Journal
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
	'(("j" "journal" plain
           "* %?"
           :target (file+head "J-%<%Y-%m-%d>.org"
                              "#+title: J-%<%Y-%m-%d>\n")
	   :empty-lines 1
	   :unnarrowed t)))

  )

;;; ghost-note.el ends here.
