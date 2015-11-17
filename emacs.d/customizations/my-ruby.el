(add-hook 'ruby-mode-hook 'robe-mode)

(require 'rspec-mode)
(setq rspec-use-rake-when-possible nil)
(setq rspec-command-options "--format progress --fail-fast")

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)
