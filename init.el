;; init.el --- Emacs configuration

;; AUTHOR: Chris Bergquist

;;; Commentary:

;;; Code:

;; INSTALL PACKAGES
;; --------------------------------------------------------------------

(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)
(require 'better-defaults)

;; BASIC CUSTOMIZATION
;; --------------------------------------------------------------------

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq inhibit-startup-message t
      initial-scratch-message nil
      backup-directory-alist (list (cons ".*" backup-dir))
      auto-save-list-file-prefix autosave-dir
      auto-save-file-name-transforms `((".*" ,autosave-dir t)))

(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)

(load-theme 'material t)
(global-linum-mode t)
(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'tools)
(require 'development)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ivy-hydra which-key web-mode use-package tide solaire-mode rudel rainbow-delimiters py-autopep8 ox-twbs org-bullets omnisharp ob-restclient neotree material-theme markdown-mode magit ledger-mode json-mode gorepl-mode go-playground gitignore-mode flymd exec-path-from-shell elpy ein doom-themes dockerfile-mode docker-compose-mode counsel-projectile company-jedi company-go better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
