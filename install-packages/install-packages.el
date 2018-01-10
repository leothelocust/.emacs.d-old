;;; install-packages.el --- Emacs Package Installation

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(better-defaults
    company
    company-go
    counsel
    counsel-projectile
    diminish
    ein
    elpy
    go-mode
    go-playground
    gorepl-mode
    flycheck
    ivy
    ivy-hydra
    magit
    material-theme
    projectile
    py-autopep8
    rainbow-delimiters
    which-key))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)

;;; install-packages.el ends here
