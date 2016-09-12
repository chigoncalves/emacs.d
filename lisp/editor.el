;;;; editor.el

(eval-when-compile
  (require 'cl)
  (require 'neotree)
  (require 'smex))

(defvar windowsp (if (eq system-type  'windows-nt)
		     t
		   nil)
  "Wheter we are running on Windows or not.")

(eval-after-load "startup"
  (fset #'display-startup-echo-area-message #'ignore))


;; Functions.

(defun --enable-speck ()
  "Configuration for `speck-mode'."
  (require 'speck)
  (setq speck-engine (quote Hunspell)
	speck-hunspell-program "/usr/bin/hunspell"
	speck-hunspell-dictionary-alist '(("pt" . "pt_PT")
					  ("en" . "en_US")
					  ("de" . "de_DE")
					  ("fr" . "fr_FR")
					  ("es" . "en_SPA"))
	speck-hunspell-default-dictionary-name "en")
  (speck-mode 1))

(defun chi-frame-lost-focus-hook ()
  "Hook to be invoked when a frame loses focus."
    (save-some-buffers t))

(defun chi-rst-mode-hook ()
  (setq rst-preferred-adornments '((?* over-and-under 1)
				   (?= simple 0)
				   (?- simple 0)
				   (?^ simple 0)
				   (?# over-and-under 2)
				   (?\" simple 0)))
  (when (and buffer-file-name
	     (string-match-p "\\(.*-\\)?not[ae]s\.rst$" buffer-file-name))
    (setq-local ispell-dictionary "portugues")))

(defun chi-before-save-hook ()
  "Hook to be called before saving a file to disk."
  (let ((directory (file-name-directory buffer-file-name)))
    (unless (file-exists-p directory)
      (make-directory directory))))

(defun chi-text-mode-hook ()
  "Hook to be called by any mode that derives from `text-mode'."
  (ruler-mode 1)
  (autopair-mode 1)
  (setq linum-format " %3d ")
  (linum-mode 1))

(defun chi-load-theme-advice (func theme &optional no-confirm no-enable)
  "Advice for procedure `load-theme'."
  (mapc #'disable-theme custom-enabled-themes)
  (funcall func theme no-confirm no-enable))

(cl-defun chi-set-key (key func &key (global nil))
  "Binds a KEY to FUNC. If GLOBAL is non nil then, the new kibing will
be available in other buffers."
  (let ((set-key-func (if global
			  #'global-set-key
			#'local-set-key)))
    (funcall set-key-func key func)))

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

(defun chi-neotree-hook ()
  "Hook to be invoked when `neotree-mode' gets activated."
  (message "Neotree...")
  (other-window 1))

(defun chi-initialize-shell-PATH ()
  "Initializes my environment path."
  (shell-command-to-string "${SHELL}"))

(defun emacs-session-filename (id)
  "Overriding this function so it genarates ids file in the
 appropriate directory. "
  (concat chi-artifacts-drectory "session." id))


;; Entry point.

(fset 'yes-or-no-p 'y-or-n-p)


(autoload 'powerline-center-theme "powerline")1

(unless windowsp
  (setq ispell-program-name "hunspell"))

(nconc completion-ignored-extensions '(".out" ".exe"))

(setq create-lockfiles nil
      eshell-directory-name chi-artifacts-drectory
      eshell-history-file-name (concat chi-artifacts-drectory
				       "eshell-history")
      eshell-aliases-file (concat chi-artifacts-drectory "eshell-aliases")
      show-paren-style 'mixed
      show-paren-delay 0.2
      org-startup-folded 'show-all
      inhibit-startup-message t
      message-log-max 5000
      display-time-interval 180
      display-time-day-and-date t
      display-time-24hr-format t
      european-calendar-style t
      calendar-week-start-day 1
      initial-major-mode 'rst-mode
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

      frame-title-format '(:eval (if buffer-file-name
				     (file-name-nondirectory buffer-file-name)
				   "%b"))

      display-time-world-list '(("Europe/Lisbon" "Lisboa")
				("Atlantic/Cape_Verde" "Praia")
				("Europe/Paris" "Nice")
				("America/New_York" "Boston")
				("Europe/Paris" "Paris")
				("Atlantic/Azores" "Açores")
				("Atlantic/Madeira" "Madeira")
				("Europe/Berlin" "Berlim")
				("Europe/Madrid" "Madrid"))
      uniquify-separator "/"
      uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 1
      ido-vertical-disable-if-short t
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right
      ido-vertical-indicator "▸"
      tab-width 2
      fill-column 69
      smex-auto-update nil
      smex-history-length 20
      smex-prompt-string "[M-x] "
      smex-save-file (concat chi-artifacts-drectory "smex-items")
      ;; initial-buffer-choice t
      blink-cursor-blinks 5
      frame-resize-pixelwise t
      window-resize-pixelwise t
      right-divider-width 5
      bottom-divider-width 5
      buffer-save-without-query t
      vc-follow-symlinks t
      sentence-end-double-space nil
      echo-keystrokes 0.3

      )

(autoload 'autopair-mode "autopair" "Autoload `autopair-mode'")
(autoload 'ido-vertical-mode "ido-vertical-mode"
  "Autoload `ido-vertical-mode'.")
(autoload 'smex-initialize "smex" "Autoload `smex'")
(autoload 'neotree-toggle "neotree" "Neotree.")
(autoload 'speck-mode "speck-mode" "Autoload `speck-mode'.")


(dolist (filename '("\\.txt$" "\\.text$" "INSTALL$" "README$"))
  (push (cons filename 'rst-mode) auto-mode-alist))

(advice-add 'load-theme :around #'chi-load-theme-advice)

;;; Variable setting/modes activation.

(show-paren-mode 1)


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
(ido-vertical-mode 1)
(smex-initialize)
(blink-cursor-mode 1)
(column-number-mode 1)
(fringe-mode '(nil . 0))
(savehist-mode)
(winner-mode)

(nconc winner-boring-buffers '("*Compile-Log*" "*Apropos*"))
(nconc ido-ignore-directories '("\\`build/" "\\`artifacts/"))
(nconc ido-ignore-buffers '("\\*WoMan-Log\\*"
				     "\\*Messages\\*"
				     "\\*Compile-Log\\*"
				     "\\*Backtrace\\*"
				     "\\*Completions\\*"
				     "\\*Help\\*"
				     "\\*wclock\\*"
				     "\\*Clang-Output\\*"
				     "\\*Clang-Error\\*"
				     "\\*Buffer List\\*"
				     "\\*Warnings\\*"))
(setq  ido-enable-flex-matching t
       ido-enable-dot-prefix t
       ido-create-new-buffer 'always
       ido-save-directory-list-file (concat chi-artifacts-drectory "ido-last"))

(nconc default-frame-alist '((font . "Office Code Pro 10")))

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

(add-hook 'rst-mode-hook #'chi-rst-mode-hook)

(chi-initialize-shell-PATH)

(chi-set-key (kbd "M-x") #'smex :global t)
(chi-set-key (kbd "<f6>") #'neotree-toggle :global t)
(chi-set-key (kbd "C-c C-c n") #'neotree-toggle :global t)

(add-hook 'neotree-mode-hook #'chi-neotree-hook)
(chi-set-key (kbd "M-S-x") 'smex-major-mode-commands :global t)
(chi-set-key (kbd "C-c s") #'cycle-spacing :global t)

(let ((info-path (getenv "CHI_INFO_DIR")))
  (when info-path
    (setq Info-additional-directory-list (split-string info-path ":"))))

(add-hook 'focus-out-hook #'chi-frame-lost-focus-hook)
(add-hook 'text-mode-hook #'chi-text-mode-hook)
(add-hook 'before-save-hook #'chi-before-save-hook)

(powerline-center-theme)

(setq powerline-gui-use-vcs-glyph t)

(load custom-file :no-error)

(provide 'editor)
