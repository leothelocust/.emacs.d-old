;;; tools.el --- Extending Emacs config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'which-key)
(which-key-setup-minibuffer)
(which-key-mode)

(require '_company)
(require '_diminish)
(require '_ivy)
(require '_magit)
(require '_projectile)



(provide 'tools)

;;; tools.el ends here
