;;; init --- Emacs Initialization

;;; Commentary:
; This is my gigantic init file.  I'll keep it as one file until this
; becomes unweildy

;;; Code:

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; Access important environment variables, i.e. $PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-initialize))

;; UI Settings
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(global-linum-mode t) ; show line numbers
(column-number-mode t) ; show current row/column in modeline
(display-time-mode t)

;; Map OSX keys correctly
(setq mac-option-modifier 'meta
      mac-command-modifier 'hyper)

(tool-bar-mode -1) ; Turn off the toolbar if the function is defined
(scroll-bar-mode -1) ; Turn off the scroll bar if the function is defined
(menu-bar-mode -1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq make-backup-files nil) ;; Don't save backups

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Theme settings
(use-package spacemacs-common
  :ensure spacemacs-theme
  :pin melpa
  :config
  (load-theme 'spacemacs-dark t))

;; Font
(set-frame-font "Hack 16" nil t)

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

;; Search settings
(use-package helm
  :ensure t
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  :bind
  ("M-x" . helm-M-x))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'grizzl)
  :config
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))


;; Keyboard settings
(setq tab-width 2
      indent-tabs-mode nil)

(defun copy-from-osx ()
  "Copy from the system clipboard."
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text)
  "Copy to the system clipboard.  TEXT is the text to copy to the clipboard."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; git settings
(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  ("C-x g" . magit-status))

;; Auto Complete config
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  :hook
  (after-init . global-company-mode) ;; Enable auto completion globally
  )

;; Syntax checking
(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode))

(use-package org
  :ensure t
  :init
  (setq org-log-done t))

;; Lisp-ish settings
(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook
  (prog-mode . rainbow-mode))

(use-package paredit
  :ensure t)

(use-package lisp-mode
  :mode ("\\.el\\'" . lisp-mode)
  :hook
  ((lisp-mode . paredit-mode)
   (lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . subword-mode)
   (lisp-mode . eldoc-mode)
   (lisp-mode . show-paren-mode)))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode))
  :hook
  ((clojure-mode . paredit-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . eldoc-mode)
   (clojurescript-mode . paredit-mode)
   (clojurescript-mode . rainbow-delimiters-mode)
   (clojurescript-mode . subword-mode)
   (clojurescript-mode . eldoc-mode)
   (clojurescript-mode . show-paren-mode)))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)
   (cider-repl-mode . rainbow-delimiters-mode)
   (cider-repl-mode . show-paren-mode)))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh"))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (multi-term spacemacs-light use-package spacemacs-theme rainbow-mode rainbow-delimiters paredit magit helm-projectile flycheck exec-path-from-shell cider auto-package-update ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
