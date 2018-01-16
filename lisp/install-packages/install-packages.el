;;; install-packages.el --- Emacs Package Installation

;; AUTHOR: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(all-the-icons
    anzu
    better-defaults
    company
    company-go
    counsel
    counsel-projectile
    diminish
    dockerfile-mode
    doom-themes
    ein
    eldoc-eval
    elpy
    gitignore-mode
    go-mode
    go-playground
    gorepl-mode
    flycheck
    iedit
    ivy
    ivy-hydra
    json-mode
    magit
    material-theme
    projectile
    py-autopep8
    rainbow-delimiters
    shrink-path
    tide
    typescript-mode
    use-package
    web-mode
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
