;;; prog-native.el

(load "tuareg-site-file")
(require 'haskell-mode-autoloads)



;; Function definitions.

(defun chi-c-mode-common-hook ()
  "A hook to be called by any mode that derives from `cc-mode'."
  (autopair-mode 1)
  (local-set-key (kbd "C-c o")
		 #'ff-find-other-file)

  (dolist (backend '(company-c-headers))
    (push backend company-backends))
  (setq-local c-ident-comments-syntactically-p t)
  (setq-local c-basic-offset 2)
  (setq-local comment-column 2)
  (chi-set-key (kbd "C-c C-h w") #'woman :global t))

(defun chi-c-mode-hook ()
  "Hook to be called when `c-mode' gets activated."
  (font-lock-add-keywords
   nil
   '(("\\<\\(begin\\|progn\\|block\\)\\(\s+\\|\n\\)?{" 1
      'font-lock-keyword-face t)
     ("\\<NUL\\>" . 'font-lock-constant-face)
     ("\\<noreturn\\>" . 'font-lock-keyword-face)
     ("\\<\\(self\\|super\\|parent\\)\\>" . 'font-lock-builtin-face))))


;; Entry point.

(defconst ce-c-style
  '(("wave"
     (c-bassic-offset . 2)
     (c-ident-comments-syntactically-p . t))))

(autoload 'cmake-mode "cmake-mode"
  "Major mode to edit CMake files.")
(push '("\\(\\.cmake\\|CMakeLists\\.txt\\)" . cmake-mode)
      auto-mode-alist)

(autoload 'cmake-font-lock-activate  "cmake-font-lock"
  "Minor mode to improve CMake highlighting.")
(autoload 'company-c-headers "company-c-headers"
  "Autoload `company-c-headers' backend for `company-mode'")

(push '("\\.h\\.in$". c-mode) auto-mode-alist)
(push '("\\.hh\\.in$". c++-mode) auto-mode-alist)
(push '("\\(\\.q\\(?:h\\(?:c?p\\)\\|rc\\)\\)" . nxml-mode)
      auto-mode-alist)

(add-hook 'c-mode-hook #'chi-c-mode-hook)
(add-hook 'c-mode-common-hook #'chi-c-mode-common-hook)
(add-hook 'cmake-mode-hook  #'cmake-font-lock-activate)


(chi-set-key (kbd "C-c C-h m") #'(lambda ()
				   (interactive)
				   (let ((woman-use-topic-at-point t))
				     (woman)))
	     :global t)

(provide 'prog-native)
