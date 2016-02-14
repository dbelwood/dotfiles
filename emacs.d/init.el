;;; init --- Emacs Initialization

;;; Commentary:
; This is my gigantic init file.  I'll keep it as one file until this
; becomes unweildy

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path "~/.emacs.d/customizations")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode -1) ; hide menubar in term
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(global-linum-mode t) ; show line numbers
(setq column-number-mode t) ; show current row/column in modeline
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(display-time-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (fboundp 'windmove-default-keybindings) ; Enable S-[up,down,left,right] window movement
  (windmove-default-keybindings))

;; Theme settings
(load-theme 'solarized t)
(defun set-background-mode (frame mode)
  "Change the background of the current editor from to light or dark.  FRAME is an editor frame.  MODE should either be 'light or 'dark."
  (set-frame-parameter frame 'background-mode mode)
  (when (not (display-graphic-p frame))
    (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
  (enable-theme 'solarized))

(defun switch-theme ()
  "Switch the theme to light or dark solarized."
  (interactive)
  (let ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
		   'light 'dark)))
    (set-background-mode nil mode)))

(custom-set-variables '(solarized-termcolors 256))
(defvar solarized-default-background-mode)
(setq solarized-default-background-mode 'dark)

(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-background-mode frame solarized-default-background-mode)))

(set-background-mode nil solarized-default-background-mode)

(global-set-key (kbd "C-c t") 'switch-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/custom-packages/helm")
(require 'helm)
(require 'helm-config)

(helm-mode 1)
(helm-adaptive-mode 1)
(helm-push-mark-mode 1)

(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)

(projectile-global-mode)
(setq projectile-enable-caching t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix tabs
(defun generate-tab-stops (&optional width max)
  "Return a tab stop sequence.  WIDTH is an optional override for the default tab stop of 2 spaces.  MAX is the max number of columns to calculate tab stops."
  (let* ((max-column (or max 100))
	 (tab-width (or width 2))
	 (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-stop-list (generate-tab-stops))
(setq-default indent-tabs-mode nil)

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
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/git/git.el")

(load-file "/usr/local/share/emacs/site-lisp/git/git.el")
(add-to-list 'load-path "~/.emacs.d/custom-packages/git-emacs")

(require 'git-blame)
(require 'magit)

(defvar magit-last-seen-setup-instructions)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x g") 'magit-status)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-dash-common-docsets '("Bash" "Clojure" "Docker" "Emacs Lisp" "Go" "Haskell" "PostgreSQL" "Racket" "Ruby" "Rust"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar auto-save-background-directory)
(setq auto-save-background-directory "~/.emacs-auto-save") ; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,auto-save-background-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-background-directory  t)))

(require 'ag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company)
(global-company-mode t)
(add-hook 'after-init-hook 'global-company-mode) ;; Enable auto completion globally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-log-done t)


(setq path-to-ctags "/usr/bin/ctags")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "my-ruby.el")
(load "my-clojure.el")
(load "my-sql.el")
(load "my-golang.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here
