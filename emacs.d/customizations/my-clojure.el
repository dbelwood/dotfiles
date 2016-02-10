(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode)

;; CIDER REPL
;(setq cider-auto-mode nil)
;(setq nrepl-log-messages t)
;(setq nrepl-hide-special-buffers t)
;(setq cider-repl-pop-to-buffer-on-connect t)
;(setq cider-show-error-buffer 'only-in-repl)
;(setq cider-auto-select-error-buffer nil)
;(setq cider-stacktrace-fill-column 80)
;(setq nrepl-buffer-name-show-port t)
;(setq cider-repl-use-pretty-printing t)
;(setq cider-switch-to-repl-command #'cider-switch-to-current-repl-buffer)
;(setq cider-repl-display-in-current-window t)

;; Re-bind history key bindings
(defun modify-keybindings ()
  (local-set-key (kbd "ESC i") #'cider-repl-backward-input)
  (local-set-key (kbd "ESC k") #'cider-repl-forward-input))

(add-hook 'nrepl-connected-hook 'modify-keybindings)
