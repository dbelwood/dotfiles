;;; init --- Emacs Initialization

;;; Commentary:
; This is my gigantic init file.  I'll keep it as one file until this
; becomes unweildy

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '(("melpa-stable" . "https://stable.melpa.org/packages/")
               ("melpa" . "https://melpa.org/packages/"))
             t)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Configuration management
;; Access important environment variables, i.e. $PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      global-linum-mode t ; show line numbers
      column-number-mode t ; show current row/column in modeline
      display-time-mode t
      )

;; Map OSX keys correctly
(setq mac-option-modifier 'meta
      mac-command-modifier 'hyper)

(tool-bar-mode -1) ; Turn off the toolbar if the function is defined
(scroll-bar-mode -1) ; Turn off the scroll bar if the function is defined
(menu-bar-mode -1)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Theme settings
(use-package spacemacs-theme
  :ensure t
  :pin melpa)

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (projectile-global-mode)
  (helm-projectile-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix tabs
;; (defun generate-tab-stops (&optional width max)
;;   "Return a tab stop sequence.  WIDTH is an optional override for the default tab stop of 2 spaces.  MAX is the max number of columns to calculate tab stops."
;;   (let* ((max-column (or max 100))
;; 	 (tab-width (or width 2))
;; 	 (count (/ max-column tab-width)))
;;     (number-sequence tab-width (* tab-width count) tab-width)))

;; (setq tab-stop-list (generate-tab-stops))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar auto-save-background-directory)
;; (setq auto-save-background-directory "~/.emacs-auto-save") ; store all backup and autosave files in the tmp dir
;; (setq backup-directory-alist
;;       `((".*" . ,auto-save-background-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,auto-save-background-directory  t)))

;; (require 'ag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :config
  (global-company-mode t)
  :hook
  (after-init . global-company-mode) ;; Enable auto completion globally
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure t
  :init
  (setq org-log-done t))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook
  (prog-mode . rainbow-mode))

(use-package paredit
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode "\\.clj*\\"
  :hook
  ((clojure-mode . paredit-mode)
   (clojure-mode . rainbow-delimiters-mode)
   (clojure-mode . subword-mode)
   (clojurescript-mode . paredit-mode)
   (clojurescript-mode . rainbow-delimiters-mode)
   (clojurescript-mode . subword-mode))))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . eldoc-mode)
   (cider-repl-mode . paredit-mode)
   (cider-repl-mode . rainbow-delimiters-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load "my-ruby.el")
;; (load "my-clojure.el")
;; (load "my-sql.el")
;; (load "my-golang.el")
;; (load "my-javascript.el")
;; ;; Racket settings
;; (setq racket-racket-program "/Applications/Racket v6.3/bin/racket")
;; (setq racket-raco-program "/Applications/Racket v6.3/bin/raco")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; debug settings
;;;(setq max-specpdl-size 5)
;;;(setq debug-on-error t)

(provide 'init)
;;; init.el ends here
