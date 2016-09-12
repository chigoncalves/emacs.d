;;; prog.el


;; Requires.

(eval-when-compile
  (require 'cl))

(require 'sr-speedbar)


;; Function definitions.
(defun chi-prog-mode-hook ()
  "Hook to be called by any mode that derives from `prog-mode'."
  (local-set-key (kbd "M-/") #'company-complete)
  (highlight-numbers-mode 1)
  (linum-mode 1)
  (ruler-mode 1)
  ;; (flyspell-prog-mode)
  (company-mode 1))

(defun chi-speedbar-mode-hook ()
  "Hook to be called when `speedbar-mode' gets activated."
  (make-face 'speedbar-face)
  (set-face-font 'speedbar-face "monospace-8")
  (buffer-face-set 'speedbar-face))

(defun doom*neo-insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (list (concat "  " (projectile-project-name))))

(defun doom*neo-insert-fold-symbol (name)
  "Custom hybrid unicode theme with leading whitespace."
  (or (and (eq name 'open)  (neo-buffer--insert-with-face " -  " 'neo-expand-btn-face))
      (and (eq name 'close) (neo-buffer--insert-with-face " +  " 'neo-expand-btn-face))
      (and (eq name 'leaf)  (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))


;; Entry point.

(fset 'html-mode 'nxml-mode)

(setq sr-speedbar-max-width 30
      sr-speedbar-width 30
      indent-tabs-mode nil)

(autoload 'company-mode "company"
  "Autoload `company-mode' text completion.")

(autoload 'highlight-numbers-mode "highlight-numbers"
  "Minor mode to highlight numbers.")

(autoload 'projectile-project-name "projectile")

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'chi-prog-mode-hook)
(add-hook 'speedbar-mode-hook #'chi-speedbar-mode-hook)

(dolist (filename '("hgignore"
		    "hgsub"
		    "hgsubstate"
		    "git/config"
		    "gitignore"
		    "gitmodules"
		    "COMMIT_EDITMSG"))
  (push (cons (concat "\\." filename) 'conf-mode) auto-mode-alist))

(advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-insert-fold-symbol)
(advice-add 'neo-buffer--insert-root-entry :filter-args 'doom*neo-insert-root-entry)

(require 'prog-lisp)
(require 'prog-misc)
(require 'prog-native)
(require 'prog-script)
(require 'prog-web)

(provide 'prog)
