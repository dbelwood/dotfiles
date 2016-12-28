;;; my-shell --- Emacs terminal settings

;;; Commentary:
;;; Terminal settings

;;; Code:
(defvar my-shells
  '("*shell0*", "*shell1*", "*shell2*", "*shell3*", "*shell4*", "*shell5*"))

(require 'tramp)

(custom-set-variables
 '(tramp-default-method "ssh")
 '(comint-scroll-to-bottom-on-input t) ; Always write text to the bottom of the screen
 '(comint-scroll-to-bottom-on-output nil) ; Always add output to the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-auto-list t)
 '(comint-input-ignoredups t)
 '(comint-completion-addsuffix t)
 '(comint-input-ignore-dups t)
 '(comint-completion-add-suffix t)
 '(comint-buffer-maximum-size 20000)
 '(comint-prompt-read-only nil)
 '(comint-get-old-input (lambda () ""))
 '(comint-input-ring-size 5000)
 '(protect-buffer-bury-p nil)
 )

(setenv "PAGER" "less")

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to 'comint-output-filter-functions to make TEXT from stdout read only in shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode()
  "Add to shell-mode-hook to use dirtrack-mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

;; interpret and use ansi colour codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

(provide 'my-shell.el)
;;; my-shell.el ends here
