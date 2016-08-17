;;;; editor.el


(eval-when-compile
  (require 'cl))

;;; autoloads.
(autoload 'autopair-mode "autopair" "Autoload `autopair-mode'")


;;; Functions definitions.

(defun chi-load-theme-advice (func theme &optional no-confirm no-enable)
  (mapc #'disable-theme custom-enabled-themes)
  (funcall func theme no-confirm no-enable))

(advice-add 'load-theme :around #'chi-load-theme-advice)

(cl-defun chi-set-key (key func &key (global nil))
  "Binds a KEY to FUNC. If GLOBAL is non nil then, the new kibing will
be available in other buffers."
  (let ((set-key-func (if global
		  #'global-set-key
		#'local-set-key)))
    (funcall set-key-func key func)))

(defun chi-path-join (&rest paths)
  "Concatenate each path in PATHS using the operating system path
separator."
  (let ((path-separator "/")
	(replace-str "/")
	full-path)
    (when (eq system-type 'windows-nt)
      (setq path-separator "\\"
	    replace-str "\\\\"))

    (setq full-path (mapconcat #'identity paths path-separator))
    (replace-regexp-in-string (format "[%s]+" path-separator)
			      replace-str
			      full-path)))

(defun chi-ignore-key (keystr-or-keylst &keys global)
  "Ignores a key or a list of keys, i.e. disables them.
KEYSTR-OR-KEYLST may be a string or a list of strings."
  (let ((fn (if global
		#'global-set-key
	      #'local-set-key)))
    (cond
     ((stringp keystr-or-keylst)
      (funcall fn (kbd keystr-or-keylst) #'ignore))
     ((listp keystr-or-keylst)
      (mapc #'(lambda (key)
		  (funcall fn (kbd key) #'ignore)) keystr-or-keylst)))
    t))

(defun chi-initialize-shell-PATH ()
  (shell-command-to-string "${SHELL}"))

;;; Overriding this function so it genarates ids file in the
;;; apropriate directory.
(defun emacs-session-filename (id)
  (concat chi-artifacts-drectory "session." id))


;;; Variable setting/modes activation.
(show-paren-mode 1)
(push '(font . "ubuntu mono-11") default-frame-alist)
(setq create-lockfiles nil
      eshell-directory-name chi-artifacts-drectory
      eshell-history-file-name (concat chi-artifacts-drectory
				       "eshell-history")
      eshell-aliases-file (concat chi-artifacts-drectory "eshell-aliases")
      show-paren-style 'mixed
      show-paren-delay 0.2
      org-startup-folded 'show-all
      major-mode 'rst-mode
      inhibit-startup-message t
      message-log-max 5000
      display-time-interval 180
      display-time-day-and-date t
      display-time-24hr-format t
      european-calendar-style t
      calendar-week-start-day 1
      save-place t
      savehist-file (concat chi-artifacts-drectory "minibuffer-history")
      history-length 15
      history-delete-duplicates t
      save-place-file (concat chi-artifacts-drectory "places")
      case-fold-search t ;; do case insensitive match.
      case-replace nil   ;; do not preserve case when replaceing text.
      current-language-environment "utf-8"
      make-backup-files nil         ;; do not create backup files.
      auto-save-default nil
      auto-save-list-file-prefix nil
      auto-save-interval 0
      auto-save-timeout 0
      mark-ring-max 10
      kill-ring-max 30
      kill-whole-line t
      global-mark-ring-max 10
      indicate-empty-lines t
      require-final-newline t
      cursor-type 'hbar
      message-truncate-lines t
      set-mark-command-repeat-pop t
      read-buffer-completion-ignore-case t
      initial-scratch-message nil
      custom-file (concat chi-artifacts-drectory "customizations.el")
      completion-ignored-extensions (append '(".out" ".exe")
					    completion-ignored-extensions)
      frame-title-format '(:eval (if buffer-file-name
				     (file-name-nondirectory buffer-file-name)
				   "%b"))
      ispell-program-name "hunspell"
      display-time-world-list '(("Europe/Lisbon" "Lisboa")
				("Atlantic/Cape_Verde" "Praia")
				("Europe/Paris" "Nice")
				("America/New_York" "Boston")
				("Europe/Paris" "Paris")
				("Atlantic/Azores" "Açores")
				("Atlantic/Madeira" "Madeira")
				("Europe/Berlin" "Berlim")
				("Europe/Madrid" "Madrid")))

(load-theme 'doom-one t)
(chi-set-key (kbd "C-c C-t d") #'display-time-world :global t)
(chi-ignore-key '( "<left>" "<right>" "<down>" "<up>" "C-x C-+" "C-x C--"
			"C-x C-n")  :global t)

(require 'saveplace)
(size-indication-mode 1)
(display-time)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(auto-save-mode -1)
(ido-mode 1)
(ido-everywhere 1)
(blink-cursor-mode 1)
(column-number-mode 1)
(fringe-mode '(nil . 0))
(savehist-mode 1)

(dolist (procedure '(tool-bar-mode scroll-bar-mode))
	(when (fboundp procedure)
	  (funcall procedure -1)))

(unless (file-exists-p chi-artifacts-drectory)
  (make-directory chi-artifacts-drectory))

;;; Do not give away clipboard ownership when exiting.
(when (display-graphic-p)
  (setf x-select-enable-clipboard-manager nil))

(chi-set-key (kbd "C-c f") #'toggle-frame-fullscreen :global t)
(chi-set-key (kbd "\r") #'newline-and-indent :global t)

(fset 'yes-or-no-p 'y-or-n-p)

(eval-after-load "startup" '(fset #'display-startup-echo-area-message #'ignore))


;;; Hooks.

(defun chi-rst-mode-hook ()
  (when (and buffer-file-name
	     (string-match-p "\\(.*-\\)?not[ae]s\.rst$" buffer-file-name))
    (setq-local ispell-dictionary "portugues")))

(add-hook 'rst-mode-hook #'chi-rst-mode-hook)

(setq  ido-enable-flex-matching t
       ido-enable-dot-prefix t
       ido-create-new-buffer 'always
       ido-save-directory-list-file
       (concat chi-artifacts-drectory "ido-history")
       ido-ignore-directories (cons "build" ido-ignore-directories)

       ido-ignore-buffers (append  '("\\*WoMan-Log\\*"
				     "\\*Messages\\*"
				     "\\*Compile-Log\\*"
				     "\\*Backtrace\\*"
				     "\\*Completions\\*"
				     "\\*Help\\*"
				     "\\*wclock\\*"
				     "\\*Clang-Output\\*"
				     "\\*Clang-Error\\*"
				     "\\*Buffer List\\*"
				     "\\*Warnings\\*")
				   ido-ignore-buffers))

(add-hook 'text-mode-hook
	  #'(lambda ()
	      (ruler-mode 1)
	      (autopair-mode 1)
	      (linum-mode 1)))

(chi-initialize-shell-PATH)

(provide 'editor)
