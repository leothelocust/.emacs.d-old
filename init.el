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
(require 'ui)

;;; init.el ends here
