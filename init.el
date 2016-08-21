;;; init.el

(when (version<= "24.5" emacs-version)
  (setq load-prefer-newer t))

;; restore to `defvar'
(defvar chi-artifacts-drectory (concat (file-name-as-directory user-emacs-directory)
				       "artifacts")
  "Directory where to store generated files.")

(setq user-emacs-directory  (file-name-as-directory (expand-file-name user-emacs-directory)))

(defun chi-load-directory (name)
  (setq name (file-name-as-directory name))
  (when (file-directory-p name)
    (let* ((directories (list name))
	  (ignore-files-pattern "^[a-zA-Z0-9_-]")
	  (ignore-directories (list "."
				    ".."
			            (file-name-nondirectory chi-artifacts-drectory)
				    "bin"
				    "dev"
				    "doc"
				    "docs"
				    "example"
				    "child-theme-example"
				    "skeleton"
				    "t"
				    "test"
				    "tests"))
	  (directory-entry-valid-p #'(lambda (entry)
				       (not (member entry ignore-directories))))
	  current-directory)
      (while (not (null directories))
	(let* ((current-directory (pop directories))
	       (directory-entries (directory-files current-directory
						   nil
						   ignore-files-pattern
						   :no-sort))
	       theme-directory-added-to-path-p
	       library-directory-added-to-path-p)
	  (dolist (directory-entry directory-entries)
	    (when (funcall directory-entry-valid-p directory-entry)
	      (setq directory-entry (concat current-directory directory-entry))

	      (cond
	       ((file-directory-p directory-entry)
		(push (file-name-as-directory directory-entry) directories))
	       ((file-regular-p directory-entry)
		(cond
		 ((string-match-p "-theme\\.el$" directory-entry)

		  (unless theme-directory-added-to-path-p
		    (setq theme-directory-added-to-path-p t)
		    (push current-directory custom-theme-load-path)))
		 ((string-match-p "\\.el$" directory-entry)
		  (unless library-directory-added-to-path-p
		    (setq library-directory-added-to-path-p t)
		    (push current-directory load-path)))))))))))))


(chi-load-directory (concat user-emacs-directory "lisp"))
(chi-load-directory (concat user-emacs-directory "vendor"))

(setq chi-artifacts-drectory (file-name-as-directory chi-artifacts-drectory))

(require 'editor)
(require 'prog)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-c C-f") #'rtags-find-symbol-at-point)



;; return-from
