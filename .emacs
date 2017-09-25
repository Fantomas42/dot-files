;; Add more packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Could be usefull to install these packages
;; html-mode sass-mode rst django-html-mode zencoding-mode yasnippet jsx-mode

;; Config base
(setq-default column-number-mode t)
(setq-default menu-bar-mode 0)
(setq-default fill-column 75)
(setq-default indent-tabs-mode nil)
(setq-default auto-save-default nil) ; stop creating those #autosave# files
(setq initial-scratch-message ";; Start coding now !")
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Aliases
(global-set-key [f6] "import pdb; pdb.set_trace()\n")
(global-set-key [f7] "import pudb; pu.db\n")

;; Hooks
(defun delete-trailing-blanklines ()
 "Deletes all blank lines at the end of the file."
 (interactive)
 (save-excursion
   (save-restriction
     (widen)
     (goto-char (point-max))
     (delete-blank-lines))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blanklines)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . jsx-mode))
;; (autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; Flymake
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
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "brightcyan"))))
 '(font-lock-string-face ((t (:foreground "green")))))
