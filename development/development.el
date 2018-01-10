;;; development.el --- Development Configuration

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'rainbow-delimiters)
(global-flycheck-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq-default indent-tabs-mode nil
              tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(require '_python)
(require '_golang)
(require '_typescript)

(provide 'development)

;;; development.el ends here
