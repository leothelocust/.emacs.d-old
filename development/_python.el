;;; _python.el --- Python Configuration

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

;; PYTHON CONFIGURATION
;; --------------------------------------------------------------------

(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501"))
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide '_python)

;;; _python.el ends here
