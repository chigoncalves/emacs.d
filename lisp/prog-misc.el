;;; prog-misc.el


;; Function definitions.


;; Entry point.

(fset 'yml-mode 'yaml-mode)

(autoload 'groovy-mode "groovy-mode" "Major mode for Groovy.")
(push '("\\(\\.gr\\(?:adle\\|oovy\\)\\)" . groovy-mode)
      auto-mode-alist)

(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown.")
(push '("\\(?:\\.m\\(?:arkdown\\|d\\)$\\)" . markdown-mode) auto-mode-alist)

(autoload 'highlight-numbers-mode "highlight-numbers"
  "Minor mode to highlight numbers.")

(autoload 'fsharp-mode "fsharp-mode"
  "Major mode for F#.")
(push '("\\.fs$" . fsharp-mode) auto-mode-alist)

(autoload 'yaml-mode "yaml-mode"
  "Major mode for YAML.")
(push '("\\.ya?ml" . yaml-mode) auto-mode-alist)

(provide 'prog-misc)
