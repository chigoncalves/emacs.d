;;; pcog-script.el

;; Functions.

(defun chi-cperl-mode-hook ()
  (ruler-mode 1))
;; 	      (autopair-mode t)
;; 	      (local-set-key (kbd "c-c c-c")
;; 			     #'cperl-comment-region)

;; 	      (local-set-key (kbd "c-c c-u")
;; 			     #'cperl-uncomment-region)

;; 	      (local-set-key (kbd "c-c h")
;; 			     #'cperl-perldoc-at-point)

;; 	      (setq-local
;; 	       cperl-electric-parens nil) ; favor autopair-mode instead.
;; 	      ;; autopair provides something better.
;; 	      (setq-local cperl-electric-parens-string nil)
;; 	      ;; automaticaly insert newline after semicolon, braces, ...
;; 	      (setq-local cperl-auto-newline t)
;; 	      ;; add a newline after a color in a label.
;; 	      (setq-local cperl-auto-newline-after-colon t)
;; 	      ;; indent after a semicolon.
;; 	      (setq-local cperl-autoindent-on-semi t)
;; 	      (setq-local cperl-break-one-line-blocks-when-indent nil)
;; 	      ;; (setq-local cperl-continued-brace-offset 0)
;; 	      (setq-local cperl-comment-column 2)
;; 	      (setq-local cperl-electric-lbrace-space nil)
;; 	      (setq-local cperl-electric-keywords t)
;; 	      (setq-local cperl-electric-parens-mark t)
;; 	      (setq-local cperl-font-lock nil)
;; 	      (setq-local cperl-brace-offset 0)
;; 	      (setq-local cperl-close-paren-offset 0)
;; 	      ;; this mode should have provide a way to indent continued
;; 	      ;; statements on the equal sign as an option.
;; 	      (setq-local cperl-continued-statement-offset 10)
;; 	      ;; do not put a space between a dolar sign and braces.
;; 	      (setq-local cperl-electric-lbrace-space nil)
;; 	      ;; do not indent comment left aligned.
;; 	      (setq-local cperl-indent-left-aligned-comments nil)
;; 	      ;; do not indent parrens ([], {}, () pairs as a block) in
;; 	      ;; annonimous hash/array or normal array/hash.
;; 	      (setq-local cperl-indent-parens-as-block nil)

;; 	      (setq-local cperl-brace-imaginary-offset 0)))

(defun chi-lua-mode-hook ()
  "Hook to be invoked when `lua-mode' gets activated."
  (setq-local lua-indent-level 2))

(defun chi-ruby-mode-hook ()
  "A hook to be invoked when `ruby-mode' gets activated."
  (setq-local ruby-indent-level 2)
  (setq-local ruby-indent-tabs-mode nil)
  (inf-ruby-minor-mode 1))

(defun chi-python-mode-hook ()
  "Hook to be invoked when `python-modes' is activated."
  (setq python-indent 2
	python-indent-offset 2
	python-indent-guess-indent-offset nil))

(defun chi-sh-mode-hook ()
  "Hook to be called when `sh-mode' gets activated."
  (setq-local sh-basic-offset 2)
  (setq-local sh-indentation 2))

(defun chi-make-script-executable ()
  "Makes the current file associated with a buffer executable on save
if it has a shebang."
  (when (and buffer-file-name
	     (not (file-executable-p buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "^#!")
    	(set-file-modes buffer-file-name #o744)))))


;; Entry point.

(fset 'perl-mode 'cperl-mode)

(defconst ce-perl-style
  '(("wave"
     (cperl-indent-level . 2)
     (cperl-brace-offset . 2)
     (cperl-brace-imaginary-offset . 0)
     (cperl-label-offset . -2)
     (cperl-indent-wrt-brace . nil)
     (cperl-brace-imaginary-offset . 0)
     (cperl-continued-brace-offset . 2)
     ;; keep braces on the same line as
     (cperl-exra-newline-before-brace  . nil)
     ;; language constructs such as : until, unless, if, etc,...
     (cperl-extra-newline-before-brace-multiline . nil)
     (cperl-extra-newline-before-brace-multiline . nil)
     (cperl-merge-trailing-else . nil))))

(autoload 'lua-mode "lua-mode"
  "Major mode for Lua programming language.")
(push '("\\.lua$" . lua-mode) auto-mode-alist)

(autoload 'inf-ruby-minor-mode "inf-ruby"
  "Inferior Ruby mode.")

(add-hook 'cperl-mode-hook #'chi-cperl-mode-hook)
(add-hook 'lua-mode-hook #'chi-lisp-mode-hook)
(add-hook 'python-mode-hook #'chi-python-mode-hook)

(add-hook 'ruby-mode-hook #'chi-ruby-mode-hook)
(add-hook 'sh-mode-hook #'chi-sh-mode-hook)
(add-hook 'after-save-hook #'chi-make-script-executable)

(provide 'prog-script)
