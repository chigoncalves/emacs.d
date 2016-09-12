;;; prog-web.el

;; Function definitions.

(defun chi-nxml-mode-hook ()
  "A hook be called when `nxml-mod' is activated."
  (autopair-mode t)
  (linum-mode 1))

(defun chi-css-mode-hook ()
  "A hook to be invoked when `css-mode' gets activated."
  (linum-mode 1)
  (autopair-mode t)
  (setq-local css-indent-offset 2))

(defun chi-web-mode-hook ()
  "Hook to be invoked when `css-mode' get activated."
  (setq-local web-mode-code-indent-offset 2)
  (setq-local web-mode-css-indent-offset 2)
  (setq-local web-mode-markup-indent-offset 2))

(defun chi-js-mode-hook ()
  "Hook to be called when `js-mode' gets activated."
  (setq-local js-indent-level 2))


;; Entry point.

(autoload 'php-mode "php-mode"
  "Major mode for PHP programming language.")
(push '("\\.php$" . php-mode) auto-mode-alist)
(push '("\\.template\\.php$" . web-mode) auto-mode-alist)

(fset 'js-mode 'js2-mode)

(autoload 'web-mode "web-mode"
	  "Major mode for web development.")
(autoload 'js2-mode "js2-mode" "JS mode")

(add-hook 'nxml-mode-hook #'chi-nxml-mode-hook)
(add-hook 'css-mode-hook #'chi-css-mode-hook)
(add-hook 'web-mode-hook #'chi-web-mode-hook)
(add-hook 'js-mode-hook #'chi-js-mode-hook)

(provide 'prog-web)
