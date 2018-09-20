;;; .emacs --- Initialization file for Emacs

;;; Commentary:

;;; Fantomas42's Emacs personal config

;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Install packages
(setq package-selected-packages
      '(
        anaconda-mode
        company
        company-anaconda
        dockerfile-mode
        emmet-mode
        fill-column-indicator
        flycheck
        js2-mode
        json-mode
        helm
        helm-fuzzier
        helm-projectile
        markdown-mode
        magit
        magit-gitflow
        projectile
        smart-mode-line
        rjsx-mode
        sass-mode
        web-mode
        yaml-mode
        yasnippet
        yasnippet-snippets
	))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; Config base
(fset 'yes-or-no-p 'y-or-n-p)
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
(scroll-bar-mode -1)
(prefer-coding-system 'mule-utf-8)

;; Helm
(require 'helm)
(require 'helm-command)
(require 'helm-for-files)
(require 'helm-semantic)
(require 'helm-config)

(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-scroll-amount 8
      helm-echo-input-in-header-line t
      helm-autoresize-min-height 20
      helm-recentf-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      )

(helm-mode 1)
(helm-autoresize-mode 1)
(helm-projectile-on)
(helm-fuzzier-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(add-hook 'kill-emacs-hook #'
          (lambda ()
            (and (file-exists-p "$CONF_FILE") (delete-file "$CONF_FILE"))))

;; Web-mode
(require 'web-mode)

(setq web-mode-engines-alist '(("django" . "\\.html\\'")))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-css-colorization t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Anaconda & Company
(require 'company)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; JS2
(setq-default js2-basic-offset 2)
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-mode-show-strict-warnings nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Smart mode line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)

;; Projectile
(projectile-mode 1)

;; Yasnippet
(yas-global-mode 1)

;; Fill Column Indicator
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "#DC143C")

(defvar i42/fci-mode-suppressed nil)
(make-variable-buffer-local 'i42/fci-mode-suppressed)

(defun fci-width-workaround (frame)
  "Activate fci when terminal allow it.  FRAME."
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
;;(fci-global-mode 1)

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
 )

;;; .emacs ends here
