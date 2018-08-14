(require 'package)
(add-to-list 'package-archives  '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'better-defaults)
(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

;; General settings
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "ui.el")
(load "git.el")

(require 'flycheck)
(global-company-mode t)

;;; Language-specific settings
(add-to-list 'load-path "~/.emacs.d/customizations/languages")
(load "elixir.el")
(load "python-config.el")

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(package-selected-packages
         (quote
             (exec-path-from-shell jedi pyenv-mode-auto pyenv-mode flycheck elpy python-mode multi-term editorconfig smart-tabs-mode smartparens magit alchemist ruby-end elixir-mode color-theme-sanityinc-tomorrow better-defaults pallet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
