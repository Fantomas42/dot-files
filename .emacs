;;; .emacs --- Initialization file for Emacs

;;; Commentary:

;;; Fantomas42's Emacs personal config

;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Install packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages-list '(emmet-mode
                        fill-column-indicator
                        flycheck
                        js2-mode
                        json-mode
                        magit
                        magit-gitflow
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
(setq-default tab-width 4)
(setq-default auto-save-default nil)
(setq-default initial-scratch-message ";; Start coding now !")
(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'mule-utf-8)

;; JS2
(setq-default js2-basic-offset 2)
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-mode-show-strict-warnings nil)

;; Yasnippet
(yas-global-mode 1)

;; Fill Column Indicator
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "#DC143C")

(defvar i42/fci-mode-suppressed nil)
(make-variable-buffer-local 'i42/fci-mode-suppressed)

(defun fci-width-workaround (frame)
  "Activate fci when terminal allow it."
  (let ((fci-enabled (symbol-value 'fci-mode))
        (fci-column (if fci-rule-column fci-rule-column fill-column))
        (current-window-list (window-list frame 'no-minibuf)))
    (dolist (window current-window-list)
      (with-selected-window window
        (if i42/fci-mode-suppressed
            (when (and (eq fci-enabled nil)
                       (< fci-column
                          (+ (window-width) (window-hscroll))))
              (setq i42/fci-mode-suppressed nil)
              (turn-on-fci-mode))
          ;; i42/fci-mode-suppressed == nil
          (when (and fci-enabled fci-column
                     (>= fci-column
                         (+ (window-width) (window-hscroll))))
            (setq i42/fci-mode-suppressed t)
            (turn-off-fci-mode)))))))
(add-hook 'window-size-change-functions 'fci-width-workaround)

(define-globalized-minor-mode fci-global-mode fci-mode
  (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (string-match "^magit.*" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
(fci-global-mode 1)

;; Aliases
(global-set-key [f6] "import pdb; pdb.set_trace()\n")
(global-set-key [f7] "import pudb; pu.db\n")
(global-set-key (kbd "C-x g") 'magit-status)

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
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
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
 '(package-selected-packages
   (quote
    (magit-gitflow magit yasnippet-snippets sass-mode rjsx-mode python-mode json-mode flycheck fill-column-indicator emmet-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
