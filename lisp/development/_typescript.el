;;; _typescript.el --- TypeScript Config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'typescript-mode)
(require 'tide)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
          '(lambda ()
             (set (make-local-variable 'company-backends) '(company-tide))
             (setq company-tooltip-limit 20
                   company-idle-delay .3
                   company-echo-delay 0
                   company-begin-commands '(self-insert-command)
                   tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
             (tide-setup)))

(provide '_typescript)

;;; _typescript.el ends here
