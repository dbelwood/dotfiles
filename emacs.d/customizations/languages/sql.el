;;; sql --- SQL settings

;;; Commentary:
;;; .sql/.sqlx settings

;;; Code:

(setq sql-connection-alist '())
(defun dbb/local-database ()
  (interactive)
  (dbb/sql-connect 'postgres 'local))

(defun dbb/sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))

(defun modify-keybindings ()
  (local-set-key (kbd "ESC <up>") #'comint-previous-matching-input-from-input)
  (local-set-key (kbd "ESC <down>") #'comint-next-matching-input-from-input))

(add-hook 'sql-interactive-mode-hook 'modify-keybindings)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(provide 'sql)
;;; sql.el ends here
