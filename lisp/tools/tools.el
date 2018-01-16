;;; tools.el --- Extending Emacs config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'which-key)
(which-key-setup-minibuffer)
(which-key-mode)

(load "_company")
(load "_diminish")
(load "_ivy")
(load "_magit")
(load "_projectile")

(provide 'tools)

;;; tools.el ends here
