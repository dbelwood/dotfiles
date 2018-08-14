;;; elixir --- Elixir initialization

;;; Commentary:
;;; ex/exs settings

;;; Code:
(require 'elixir-mode)
(require 'ruby-end)
(require 'alchemist)
(require 'smartparens)

(sp-with-modes '(elixir-mode)
	       (sp-local-pair "fn" "end"
			      :when '(("SPC" "RET"))
			      :actions '(insert navigate))
	       (sp-local-pair "do" "end"
			      :when '(("SPC" "RET"))
			      :post-handlers '(sp-ruby-def-post-handler)
			      :actions '(insert navigate)))

(add-to-list 'elixir-mode-hook
	     (defun auto-activate-ruby-end-mode-for-elixir-mode ()
	       (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
		    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
	       (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
	       (ruby-end-mode +1)))

(provide 'elixir)
;;; elixir.el ends here
