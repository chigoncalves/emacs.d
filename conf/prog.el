;;;; prog.el


;;; Requires.

(eval-when-compile
  (require 'cl))

(require 'geiser)
(require 'sly-autoloads)
(require 'sr-speedbar)

(eval-after-load 'php-mode
  '(require 'web-mode))


;;; Autoloads.

(autoload 'company-mode "company"
	  "Autoload `company-mode' text completion.")
(autoload 'company-c-headers "company-c-headers"
	  "Autoload `company-c-headers' backend for `company-mode'")
(autoload 'highlight-numbers-mode "highlight-numbers"
	  "Minor mode to highlight numbers.")
(autoload 'highlight-quoted-mode "highlight-quoted"
	  "Minor mode to highlight quoted symbols.")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters"
	  "Minor mode to highlight parens.")
(autoload 'cmake-mode "cmake-mode"
	  "Major mode to edit CMake files.")
(autoload 'cmake-font-lock-activate  "cmake-font-lock"
	  "Minor mode to improve CMake highlighting.")
(autoload 'web-mode "web-mode"
	  "Major mode for web development.")
(autoload 'lua-mode "lua-mode"
	  "Major mode for Lua programming language.")
(autoload 'php-mode "php-mode"
	  "Major mode for PHP programming language.")
(autoload 'yaml-mode "yaml-mode"
	  "Major mode for YAML.")
(autoload 'js2-mode "js2-mode" "JS mode")
(autoload 'groovy-mode "groovy-mode" "Major mode for Groovy.")
(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure.")
(autoload 'eulisp-mode "eulisp-mode" "Major mode for Eulisp.")
(autoload 'newlisp-mode "newlisp-mode" "Major mode for Newlisp.")
(autoload 'enable-paredit-mode "paredit" "Enable ParEdit.")
(autoload 'company-scheme-backend "company-scheme"
  "Company backend for Sceme.")
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown.")
(autoload 'racket-mode "racket-mode" "Major mode for Racket.")

;;; File extension association.

(push '("\\(\\.cmake\\|CMakeLists\\.txt\\)" . cmake-mode)
      auto-mode-alist)
(push '("\\.h\\.in$". c-mode) auto-mode-alist)
(push '("\\.hh\\.in$". c++-mode) auto-mode-alist)
(push '("\\.lua$" . lua-mode) auto-mode-alist)
(push '("\\.php$" . php-mode) auto-mode-alist)
(push '("\\.rkt$" . scheme-mode) auto-mode-alist)
(push '("\\(\\.q\\(?:h\\(?:c?p\\)\\|rc\\)\\)" . nxml-mode)
      auto-mode-alist)
(push '("\\.ya?ml" . yaml-mode) auto-mode-alist)
(push '("\\(\\.gr\\(?:adle\\|oovy\\)\\)" . groovy-mode)
      auto-mode-alist)
(push '("\\.clj$" . clojure-mode) auto-mode-alist)
(push '("\\.e[ms]$" . eulisp-mode)
      auto-mode-alist)
(push '("\\(\\.l\\(?:sp\\)?$\\)" . newlisp-mode) auto-mode-alist)
(push '("\\(?:\\.m\\(?:arkdown\\|d\\)$\\)" . markdown-mode) auto-mode-alist)

(dolist (filename '("hgignore" "git/config" "gitignore" "gitmodules"))
  (push (cons (concat "\\." filename) 'conf-mode) auto-mode-alist))

(push '("\\.rkt$" . racket-mode) auto-mode-alist)

(fset 'elisp-mode 'emacs-lisp-mode)
(fset 'js-mode 'js2-mode)
(fset 'perl-mode 'cperl-mode)
(fset 'yml-mode 'yaml-mode)
(fset 'clj-mode 'clojure-mode)

;; Do not use tabs for indenting, use spaces only.
(setq indent-tabs-mode nil)

(defconst ce-c-style
  '(("wave"
     (c-bassic-offset . 2)
     (c-ident-comments-syntactically-p . t))))

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

(add-hook 'cmake-mode-hook  #'cmake-font-lock-activate)

;;"Generic hook for any mode derived from `c-mode'."
(add-hook 'c-mode-common-hook
	  #'(lambda ()
	      (autopair-mode 1)
	      (local-set-key (kbd "C-c o")
			     #'ff-find-other-file)

	      (dolist (backend '(company-c-headers))
		      (push backend company-backends))
	      (setq-local c-ident-comments-syntactically-p t)
	      (setq-local c-basic-offset 2)
	      (setq-local comment-column 2)))

(add-hook 'prog-mode-hook
	  #'(lambda ()
	     (local-set-key (kbd "M-/") #'company-complete)
	     (highlight-numbers-mode 1)
	     (linum-mode 1)
	     (ruler-mode 1)
	     (flyspell-prog-mode)
	     (company-mode 1)))

(add-hook 'text-mode-hook #'(lambda()
			      (flyspell-mode 1)))

(defun chi-lisp-mode-hook ()
  "Generic hook for languages in the lisp family."
  (enable-paredit-mode)
  (ruler-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-quoted-mode 1))

(dolist (mode '(emacs-lisp-mode-hook lisp-mode-hook eulisp-mode-hook
		newlisp-mode-hook clojure-mode-hook scheme-mode-hook
		geiser-mode-hook ielm-mode-hook))
	(add-hook mode #'chi-lisp-mode-hook))

(add-hook 'lisp-mode-hook #'sly-editing-mode)

(defun chi-compile-current-buffer-file ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (let* ((ignore-files-pattern '("init\\.el$" "\\.dir-locals\\.el$"))
	   (compiled-file
	    (replace-regexp-in-string "\\.el" ".elc" buffer-file-name)))

      (if (and buffer-file-name
	       (not (cl-member buffer-file-name ignore-files-pattern :test
			       #'(lambda (first second)
				   (string-match second
						 first))))

	       (or (not (file-exists-p compiled-file))
		   (file-newer-than-file-p buffer-file-name compiled-file)))
	  (byte-compile-file buffer-file-name)))))


(add-hook 'before-save-hook #'delete-trailing-whitespace)

(add-hook 'after-save-hook #'chi-compile-current-buffer-file)

(add-hook 'ruby-mode-hook
	  #'(lambda ()
	      (setq-local ruby-indent-level 2)
	      (setq-local ruby-indent-tabs-mode nil)
	      (eldoc-mode 1)))

(add-hook 'python-mode-hook
	  #'(lambda ()
	      (setq-local python-indent-offset 2)))

(add-hook 'css-mode-hook
	  #'(lambda ()
	      (linum-mode 1)
	      (autopair-mode t)
	      (setq-local css-indent-offset 2)))

(add-hook 'html-mode-hook
	  #'(lambda ()
	      (autopair-mode t)
	      (linum-mode 1)))


(add-hook 'cperl-mode-hook
	  #'(lambda()
	      (ruler-mode 1)))
;; 	      (autopair-mode t)
;; 	      (local-set-key (kbd "C-c C-c")
;; 			     #'cperl-comment-region)

;; 	      (local-set-key (kbd "C-c C-u")
;; 			     #'cperl-uncomment-region)

;; 	      (local-set-key (kbd "C-c h")
;; 			     #'cperl-perldoc-at-point)

;; 	      (setq-local
;; 	       cperl-electric-parens nil) ; favor autopair-mode instead.
;; 	      ;; autopair provides something better.
;; 	      (setq-local cperl-electric-parens-string nil)
;; 	      ;; automaticaly insert newline after semicolon, braces, ...
;; 	      (setq-local cperl-auto-newline t)
;; 	      ;; Add a newline after a color in a label.
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
;; 	      ;; This mode should have provide a way to indent continued
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

(add-hook 'lua-mode-hook
	  #'(lambda ()
	      (setq-local lua-indent-level 2)))

(add-hook 'web-mode-hook
	  #'(lambda ()
	      (setq-local web-mode-code-indent-offset 2)
	      (setq-local web-mode-css-indent-offset 2)
	      (setq-local web-mode-markup-indent-offset 2)))

(add-hook 'js-mode-hook
	  #'(lambda ()
	      (setq-local js-indent-level 2)))

(add-hook 'sh-mode-hook
	  #'(lambda ()
	      (setq-local sh-basic-offset 2)
	      (setq-local sh-indentation 2)))

(setq geiser-guile-load-init-file-p t)
(setq geiser-repl-history-filename (chi-path-join
				    +chi-emacs-gen-dir+
				    "geiser-history"))
(setq geiser-active-implementations '(guile racket))
(setq geiser-repl-history-size 1000)
(setq geiser-repl-query-on-kill-p nil)
;; (setq geiser-autodoc-identifier-format "%s →%s")
(setq geiser-autodoc-identifier-format "%s / %s")
(setq geiser-guile-warning-level 'high)
(setq geiser-edit-symbol-method 'window)
(setq geiser-mode-smart-tab-p t)
(add-hook 'geiser-mode-hook
	  #'(lambda ()
	      (setq geiser-repl-use-other-window t)
	      (setq-local geiser-repl-forget-old-errors-p t)

	      (setq-local company-backends
			  '(geiser-company-backend))))

(setq sly-default-lisp 'ccl
      sly-lisp-implementations '((ccl ("ccl" "-Q"))
				 (abcl ("abcl-rl"))
				 (sbcl ("sbcl"))
				 (cmucl ("cmucl" "-quiet"))))

(add-hook 'lisp-mode-hook #'sly-mode)

(add-hook 'c-mode-hook #'(lambda ()
			   (font-lock-add-keywords
			    nil
			    '(("\\<\\(begin\\|progn\\|block\\)\\(\s+\\|\n\\)?{" 1
			       'font-lock-keyword-face t)
			      ("\\<NUL\\>" . 'font-lock-constant-face)
			      ("\\<noreturn\\>" . 'font-lock-keyword-face)
			      ("\\<\\(self\\|super\\|parent\\)\\>" . 'font-lock-builtin-face)))))

(defun chi-make-script-executable ()
  "Makes the current file associated with a buffer executable on save
if it has a shebang."
  (when (and buffer-file-name
	     (not (file-executable-p buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (when (looking-at "^#!")
    	(set-file-modes buffer-file-name #o744)))))

(add-hook 'after-save-hook #'chi-make-script-executable)

(defun chi-speedbar-mode-hook ()
  (make-face 'speedbar-face)
  (set-face-font 'speedbar-face "monospace-8")
  (buffer-face-set 'speedbar-face))

(setq sr-speedbar-max-width 30
      sr-speedbar-width 30)

(add-hook 'speedbar-mode-hook #'chi-speedbar-mode-hook)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(chi-set-key (kbd "C-c C-h m") #'(lambda ()
				   (interactive)
				   (let ((woman-use-topic-at-point t))
				     (woman)))
	     :global t)

(chi-set-key (kbd "C-c C-h w") #'woman :global t)

(defun chi-scheme-mode-hook ()
  (make-local-variable 'company-backends)
  (setq-local company-backends
	      '(company-scheme-backend geiser-company-backend))
	      ;; (geiser-mode t)
)

(add-hook 'scheme-mode-hook #'chi-scheme-mode-hook)


(setq scheme-program-name "guile"
      geiser-mode-auto-p nil)

(provide 'prog)
