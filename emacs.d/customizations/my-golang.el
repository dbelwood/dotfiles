(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/dougm/goflymake"))
(require 'go-flymake)
(require 'go-flycheck)
