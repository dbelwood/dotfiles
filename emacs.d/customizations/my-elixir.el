;;; my-elixir --- Elixir Initialization

;;; Commentary:
;;; ex/exs settings

;;; Code:

(require 'elixir-mode)
(require 'ruby-end)
(require 'alchemist)
;; (require 'cl)
(require 'smartparens-config)

(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(:add my-elixir-do-end-close-action)
         :actions '(insert)))

(sp-with-modes '(elixir-mode)
	       (sp-local-pair "fn" "end"
			      :when '(("SPC" "RET"))
			      :actions '(insert navigate))
	       (sp-local-pair "do" "end"
			      :when '(("SPC" "RET"))
			      :post-handlers '(sp-ruby-def-post-handler)
			      :actions '(insert navigate)))

;; (setq alchemist-hooks-compile-on-save t)

(add-to-list 'elixir-mode-hook
	     (defun auto-activate-ruby-end-mode-for-elixir-mode ()
	       (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
		    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
	       (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
	                      (ruby-end-mode +1)))


;; (add-to-list 'alchemist-mode-hook
;;              (defun alchemist-company--wait-for-doc-buffer ()
;;                (defvar alchemist-company--countdown)
;;                (setf alchemist-company--countdown 50)
;;                (while (and (not alchemist-company-doc-lookup-done)
;;                            (> (cl-decf alchemist-company--countdown) 1))
;;                  (sit-for 0.01))))
(provide 'my-elixir)
;;; my-elixir.el ends here
