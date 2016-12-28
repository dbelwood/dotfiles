;;; my-ruby --- Ruby customizations

;;; Commentary:
;;; Ruby settings

;;; Code:

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(setq enh-ruby-program (concat (getenv "RUBY_ROOT") "/bin/ruby"))
(setq enh-ruby-deep-indent-paren t)
(setq enh-ruby-bounce-deep-indent t)
(require 'robe)
(eval-after-load 'company
  '(push 'company-robe company-backends))
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(require 'rspec-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-mode)
(require 'chruby)
(add-hook 'enh-ruby-mode-hook 'chruby-use-corresponding)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)


(setq rspec-use-rake-when-possible nil)
(setq rspec-command-options "--format progress --fail-fast")

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(provide 'my-ruby)
;;; my-ruby.el ends here
