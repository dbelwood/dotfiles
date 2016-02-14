;;; my-clojure --- Clojure(Script) Intialization

;;; Commentary:
;;; clj/cjlx/cljs settings

;;; Code:

;;; Parentheses management
(require 'rainbow-delimiters)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojurescript-mode-hook #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; CIDER REPL
(setq cider-pprint-fn 'puget)
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

(defun modify-keybindings ()
  "Re-bind history key bindings."
  (local-set-key (kbd "ESC <up>") #'cider-repl-backward-input)
  (local-set-key (kbd "ESC <down>") #'cider-repl-forward-input))
  (local-set-key (kbd "ESC i") #'cider-repl-backward-input)
  (local-set-key (kbd "ESC k") #'cider-repl-forward-input))

(add-hook 'nrepl-connected-hook 'modify-keybindings)

(provide 'my-clojure)
;;; my-clojure.el ends here
