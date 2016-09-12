;;; prog-lisp.el


;; Requires/loads.

(require 'geiser)
(require 'sly-autoloads)


;; Function definitions.

(defun chi-lisp-common-hook ()
  "Generic hook for languages in the Lisp family."
  (autopair-mode -1)
  (enable-paredit-mode)
  (ruler-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-quoted-mode 1))

(defun chi-compile-current-buffer-file ()
  "This function compiles any ELisp file on save."
  (interactive)
  (when (and buffer-file-name (eq major-mode 'emacs-lisp-mode)
	     (string-match-p chi-lisp-directory buffer-file-name)
    (let ((compiled-file (replace-regexp-in-string "\\.el"
						    ".elc"
						    buffer-file-name))
	  (file-basename (file-name-nondirectory buffer-file-name)))
      (if (and (not (member file-basename
			    chi--files-to-ignore-when-compiling))
	       (or (not (file-exists-p compiled-file))
		   (file-newer-than-file-p buffer-file-name compiled-file)))
	  (byte-compile-file buffer-file-name))))))

(defun chi-geiser-mode-hook ()
  "Hook to be invoked when `geiser-mod' gets activated."
  (setq geiser-repl-use-other-window t)
  (setq-local geiser-repl-forget-old-errors-p t)
  (setq-local company-backends
	      '(geiser-company-backend)))

(defun chi-scheme-mode-hook ()
  "Hook to be invoked when `scheme-mode' gets activated."
  (make-local-variable 'company-backends)
  (setq-local company-backends
	      '(company-scheme-backend geiser-company-backend)))

(defun chi-emacs-lisp-hook ()
  "Hook to be run after activating `emacs-lisp-mode'."
  (eldoc-mode 1)
  (setq eldoc-idle-delay 2)
  (checkdoc-minor-mode 1)
  (company-quickhelp-mode 1))


;; Entry point.

(fset 'elisp-mode 'emacs-lisp-mode)
(fset 'clj-mode 'clojure-mode)

(defvar chi--files-to-ignore-when-compiling  '("init.el"
					       ".dir-locals.el"
					       "c11-mode.el")
  "Files to be ignore by `chi-compile-current-buffer-file'.")

(setq sly-default-lisp 'ccl
      sly-lisp-implementations '((ccl ("ccl" "-Q"))
				 (abcl ("abcl-rl"))
				 (sbcl ("sbcl"))
				 (cmucl ("cmucl" "-quiet")))
      scheme-program-name "guile"
      geiser-mode-auto-p nil
      compilation-scroll-output t)

(setq geiser-guile-load-init-file-p t
      geiser-repl-history-filename (concat
				    chi-artifacts-drectory
				    "geiser-history")
      geiser-active-implementations '(guile racket)
      geiser-repl-history-size 1000
      geiser-repl-query-on-kill-p nil
      geiser-autodoc-identifier-format "%s / %s"
      geiser-guile-warning-level 'high
      geiser-edit-symbol-method 'window
      geiser-mode-smart-tab-p t)

(autoload 'clojure-mode "clojure-mode" "Major mode for Clojure.")
(push '("\\.clj$" . clojure-mode) auto-mode-alist)

(autoload 'company-scheme-backend "company-scheme"
  "Company backend for Sceme.")

(autoload 'enable-paredit-mode "paredit" "Enable ParEdit.")

(autoload 'eulisp-mode "eulisp-mode" "Major mode for Eulisp.")
(push '("\\.e[ms]$" . eulisp-mode) auto-mode-alist)

(autoload 'highlight-quoted-mode "highlight-quoted"
	  "Minor mode to highlight quoted symbols.")

(autoload 'newlisp-mode "newlisp-mode" "Major mode for Newlisp.")
(push '("\\(\\.l\\(?:sp\\)?$\\)" . newlisp-mode) auto-mode-alist)

(autoload 'racket-mode "racket-mode" "Major mode for Racket.")
(push '("\\.rkt$" . racket-mode) auto-mode-alist)

(autoload 'company-quickhelp-mode "company-quickhelp")

(autoload 'rainbow-delimiters-mode "rainbow-delimiters"
  "Minor mode to highlight parens.")

(add-hook 'geiser-mode-hook #'chi-geiser-mode-hook)
(add-hook 'scheme-mode-hook #'chi-scheme-mode-hook)
(add-hook 'emacs-lisp-mode-hook #'chi-emacs-lisp-hook)
(add-hook 'lisp-mode-hook #'sly-mode)
(add-hook 'after-save-hook #'chi-compile-current-buffer-file)

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook
                                     eulisp-mode-hook
                                     newlisp-mode-hook
                                     clojure-mode-hook
                                     scheme-mode-hook
                                     geiser-mode-hook
                                     ielm-mode-hook))
  (add-hook hook #'chi-lisp-common-hook))

(add-hook 'lisp-mode-hook #'sly-editing-mode)

(provide 'prog-lisp)
