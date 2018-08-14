;;; ui --- General UI settings

;;; Commentary:
;;; UI settings

;;; Code:

(global-linum-mode t)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Display time
(setq display-time-string-forms
    '((propertize
          (format-time-string " %H:%M " now)
          'face 'mode-line
          'help-echo (format-time-string "%H:%M" now))))
(display-time-mode t)
;;; Display time

(require 'editorconfig)
(editorconfig-mode 1)

;;; Theme
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme night)
;;; Theme

;;; Fonts
(add-to-list 'default-frame-alist '(font . "Hack-14"))
;;; Fonts

(provide 'ui)
;;; ui.el ends here
