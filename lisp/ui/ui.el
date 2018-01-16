;;; ui.el --- config specific to UI

;;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(load "_modeline")

(cond ((member "Essential PragmataPro" (font-family-list))
       (set-face-attribute 'default nil :font "Essential PragmataPro-14")))

(provide 'ui)

;;; ui.el ends here
