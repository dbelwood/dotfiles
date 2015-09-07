(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

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
  (local-set-key (kbd "ESC <up>") #'cider-repl-backward-input)
  (local-set-key (kbd "ESC <down>") #'cider-repl-forward-input))

(add-hook 'nrepl-connected-hook 'modify-keybindings)

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))
