;; init.el --- Emacs configuration

;; AUTHOR: Chris Bergquist

;;; Commentary:

;;; Code:

;; INSTALL PACKAGES
;; --------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/development")
(add-to-list 'load-path "~/.emacs.d/lisp/install-packages/")
(add-to-list 'load-path "~/.emacs.d/lisp/private/")
(add-to-list 'load-path "~/.emacs.d/lisp/tools/")
(add-to-list 'load-path "~/.emacs.d/lisp/ui/")

(load "install-packages")
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

(load-theme 'doom-one t)
(global-linum-mode t)
(global-auto-revert-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(load "private")
(load "tools")
(load "development")
(load "ui")

;;; init.el ends here
