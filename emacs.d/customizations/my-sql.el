(setq sql-connection-alist
      '((local (sql-product 'postgres)
               (sql-port 5432)
               (sql-server "trunkclub.dev")
               (sql-user "trunkclub")
               (sql-database "postgres"))))

(defun tc/local-database ()
  (interactive)
  (tc/sql-connect 'postgres 'local))

(defun tc/sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))

(defun modify-keybindings ()
  (local-set-key (kbd "ESC <up>") #'comint-previous-matching-input-from-input)
  (local-set-key (kbd "ESC <down>") #'comint-next-matching-input-from-input))

(add-hook 'sql-interactive-mode-hook 'modify-keybindings)
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
