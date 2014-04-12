(setq my-lisp-directory (expand-file-name "~/.emacs-lisp/"))
(add-to-list 'load-path my-lisp-directory)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'haml-mode)
(require 'sass-mode)
(require 'rst)
(require 'django-html-mode)
(require 'django-mode)
(require 'zencoding-mode)
(require 'yasnippet)

(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

(column-number-mode 1)
(menu-bar-mode 0)
(setq-default fill-column 75)
(global-set-key [f6] "import pdb; pdb.set_trace()\n")
(global-set-key [f7] "import pudb; pu.db\n")
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "flake8" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
