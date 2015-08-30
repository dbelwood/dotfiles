;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
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
(display-time-mode t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (fboundp 'windmove-default-keybindings) ; Enable S-[up,down,left,right] window movement
  (windmove-default-keybindings))

;; Theme settings
(load-theme 'solarized t)
(defun set-background-mode (frame mode)
  (set-frame-parameter frame 'background-mode mode)
  (when (not (display-graphic-p frame))
    (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
  (enable-theme 'solarized))

(defun switch-theme ()
  (interactive)
  (let ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
		   'light 'dark)))
    (set-background-mode nil mode)))

(custom-set-variables '(solarized-termcolors 256))
(setq solarized-default-background-mode 'dark)

(add-hook 'after-make-frame-functions
	  (lambda (frame) (set-background-mode frame solarized-default-background-mode)))

(set-background-mode nil solarized-default-background-mode)

(global-set-key (kbd "C-c t") 'switch-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex settings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Interactive support
(require 'ido)
(ido-mode t)

;; Evil mode settings
(setq evil-toggle-key "C-q")
(evil-mode 1)
(global-evil-surround-mode 1)

;; fix tabs
(defun generate-tab-stops (&optional width max)
  "Return a tab stop sequence"
  (let* ((max-column (or max 100))
	 (tab-width (or width tab-width))
	 (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 2)
(setq tab-stop-list (generate-tab-stops))
(setq-default indent-tabs-mode nil)

(defun copy-from-osx ()
  "Copy from the system clipboard"
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  "Copy to the system clipboard"
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
(add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs")

(load-file "/usr/local/share/git-core/contrib/emacs/git.el")

(require 'git-blame)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x g") 'magit-status)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store all backup and autosave files in the tmp dir
(setq auto-save-background-directory "~/.emacs-auto-save")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "ruby.el")
(load "clojure.el")
(load "sql.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
