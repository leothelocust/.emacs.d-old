;;; _magit.el --- Magit Config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

(provide '_magit)

;;; _magit.el ends here
