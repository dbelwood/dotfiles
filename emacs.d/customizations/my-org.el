;;; my-org --- Org mode settings

;;; Commentary:
;;; Org mode settings

;;; Code:

(require 'org)

;;; fontify code in code blocks
(setq org-src-fontify-natively t)


(org-block-begin-line
 ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
(org-block-background
 ((t (:background "#FFFFEA"))))
(org-block-end-line
 ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))

(provide 'my-org)
;;; my-org.el ends here
