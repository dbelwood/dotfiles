(require 'elixir-mode)
(require 'ruby-end)
(require 'alchemist)

(sp-with-modes '(elixir-mode)
	       (sp-local-pair "fn" "end"
			      :when '(("SPC" "RET"))
			      :actions '(insert navigate))
	       (sp-local-pair "do" "end"
			      :when '(("SPC" "RET"))
			      :post-handlers '(sp-ruby-def-post-handler)
			      :actions '(insert navigate)))

(defun elixir-mode-compile-on-save ()
  "Elixir mode compile files on save."
  (and (file-exists-p (buffer-file-name))
       (file-exists-p (elixir-mode-compiled-file-name))
       (elixir-cos-mode t)))
(add-hook 'elixir-mode-hook 'elixir-mode-compile-on-save)

(add-to-list 'elixir-mode-hook
	     (defun auto-activate-ruby-end-mode-for-elixir-mode ()
	       (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
		    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
	       (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
	                      (ruby-end-mode +1)))
