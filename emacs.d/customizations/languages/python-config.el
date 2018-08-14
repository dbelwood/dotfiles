;;; python-config --- Python settings

;;; Commentary:
;;; Python settings

;;; Code:
(require 'pyenv-mode-auto)
(pyenv-mode)
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'python-config)
;;; python.el ends here
