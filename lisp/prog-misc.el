;;; prog-misc.el


;; Entry point.

(fset 'yml-mode 'yaml-mode)

(autoload 'groovy-mode "groovy-mode" "Major mode for Groovy.")
(push '("\\.gradle$" . groovy-mode) auto-mode-alist)
(push '("\\.groovy$" . groovy-mode) auto-mode-alist)

(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown.")
(push '("\\(?:\\.m\\(?:arkdown\\|d\\)$\\)" . markdown-mode) auto-mode-alist)

(autoload 'fsharp-mode "fsharp-mode" "Major mode for F#.")
(push '("\\(\\.fs\\(?:script\\|[ix]\\)?$\\)" . fsharp-mode) auto-mode-alist)

(autoload 'yaml-mode "yaml-mode"
  "Major mode for YAML.")
(push '("\\.ya?ml" . yaml-mode) auto-mode-alist)

(provide 'prog-misc)
