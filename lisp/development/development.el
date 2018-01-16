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

(electric-pair-mode 1)
(show-paren-mode 1)

(load "_python")
(load "_golang")
(load "_typescript")

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile*\\'" . dockerfile-mode))

(require 'gitignore-mode)
(add-to-list 'auto-mode-alist '("gitignore\\'" . gitignore-mode))

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

(provide 'development)

;;; development.el ends here
