;;; _projectile.el --- Projectile Config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'counsel-projectile)

(projectile-mode)
(setq projectile-mode-line '(:eval (format " %s" (projectile-project-name)))
      projectile-remember-window-configs t
      projectile-completion-system 'ivy)
(counsel-projectile-mode)

(provide '_projectile)

;;; _projectile.el ends here
