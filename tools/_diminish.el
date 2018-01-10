;;; _diminish.el --- diminish config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'diminish)
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "counsel" '(diminish 'counsel-mode))
(eval-after-load "elpy" '(diminish 'elpy-mode))
(eval-after-load "go-mode" '(diminish 'go-mode))
(eval-after-load "go-playground" '(diminish 'go-playground-mode))
(eval-after-load "gorepl-mode" '(diminish 'gorepl-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))

(provide '_diminish)

;;; _diminish.el ends here
