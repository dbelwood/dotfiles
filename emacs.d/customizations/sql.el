(setq sql-connection-alist
      '((local (sql-product 'postgres)
                 (sql-port 5432)
                 (sql-server "trunkclub.dev")
                 (sql-user "trunkclub")
                 (sql-password "trunkclub")
                 (sql-database "postgres"))))

(defun tc/local-database ()
  (interactive)
  (sql-connect 'postgres 'local))

(defun sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))
