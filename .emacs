;;; .emacs --- Initialization file for Emacs

;;; Commentary:

;;; Fantomas42's Emacs personal config

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Install missing packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages-list '(emmet-mode
                        fill-column-indicator
                        flycheck
                        js2-mode
                        json-mode
                        rjsx-mode
                        sass-mode
                        yasnippet
                        yasnippet-snippets)
  "A list of packages to install at launch (if needed).")

(dolist (p packages-list)
  (when (not (package-installed-p p))
    (package-install p)))

;; Config base
(setq-default column-number-mode t)
(setq-default fill-column 75)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq-default auto-save-default nil) ; stop creating those #autosave# files
(setq initial-scratch-message ";; Start coding now !")
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default js2-basic-offset 2)
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-mode-show-strict-warnings nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(menu-bar-mode -1)
;;(tool-bar-mode -1)
(prefer-coding-system 'mule-utf-8)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

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

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook (lambda () (load-theme 'manoj-dark)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blanklines)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'rst-mode-hook 'turn-on-auto-fill)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))

;; Colored #hexacolor
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{3,6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  "Colorize background with matching hexadecimal value."
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'sass-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)

;; Defaults
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

;;; .emacs ends here
