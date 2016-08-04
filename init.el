;;;; init.el

(setq load-prefer-newer t)

(require 'cl)

(defun load-path (library-path)
  (when (file-directory-p library-path)
    (let ((dirs (list library-path))
	  (curr-path nil)
	  (stored nil)
	  (lib-path-list '()))

      (while (not (= (length dirs) 0))
	(setf curr-path (pop dirs))
	(let ((dir-entries (directory-files curr-path)))
	  (setf stored nil)
	  (while (not (= (length dir-entries) 0))

	    (let ((dir-ent nil)
		  (full-path))
	      (setf dir-ent (pop dir-entries))
	      (unless (cl-member dir-ent '( "." ".." "test" "tests" "doc" "bin") :test #'string=)
		(setf full-path (concat curr-path "/" dir-ent))
		(when (file-exists-p full-path)
		  (if (file-directory-p full-path)
		      (push full-path dirs)
		    (if (string-match "\\.el$" full-path)
			(unless stored
			  (push curr-path lib-path-list)
			  (setf stored t))))))))))
      (dolist (elem lib-path-list)
	(push elem load-path)))))

(defun custom-theme-load-path (library-path)
  (when (file-directory-p library-path)
    (let ((dirs (list library-path))
	  (curr-path nil)
	  (stored nil)
	  (lib-path-list '()))

      (while (not (= (length dirs) 0))
	(setf curr-path (pop dirs))
	(let ((dir-entries (directory-files curr-path)))
	  (setf stored nil)
	  (while (not (= (length dir-entries) 0))

	    (let ((dir-ent nil)
		  (full-path))
	      (setf dir-ent (pop dir-entries))
	      (unless (cl-member dir-ent '( "." ".." "test" "tests" "doc" "bin") :test #'string=)
		(setf full-path (concat curr-path "/" dir-ent))
		(when (file-exists-p full-path)
		  (if (file-directory-p full-path)
		      (push full-path dirs)
		    (if (string-match "-theme\\.el$" full-path)
			(unless stored
			  (push curr-path lib-path-list)
			  (setf stored t))))))))))
      (dolist (elem lib-path-list)
	(push elem custom-theme-load-path)))))


(load-path "~/.emacs.d/vendor/libs")
(load-path "~/.emacs.d/vendor/modes")
(load-path "~/.emacs.d/conf")
(custom-theme-load-path "~/.emacs.d/vendor/ui")

(require 'editor)
(require 'prog)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c m") #'toggle-frame-maximized)

(global-set-key (kbd "C-c C-f") #'rtags-find-symbol-at-point)

(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d8d76abb4eb19d989d55c6a2a98e2d32ba1d5f27e12df1c84571541b910cb7da" default)))
 '(delete-selection-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
