(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Always check if package has been downloaded
(setq use-package-always-ensure t)

;; Turn off tab indentation
(setq indent-tabs-mode nil)

;; Turn off mouse inerface
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)

;; disable startup message and scratch message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; switch "yes or no" prompt to "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable confirmation if file does not exist when you use
;; C-x C-f or C-x b
(setq confirm-nonexistent-file-or-buffer nil)

;; disable confirmation when kill a buffer with a live process
;; attached to it
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

;; set tooltips to appear in echo area
(tooltip-mode 0)

;; highlight current line
(global-hl-line-mode 1)

;; display column number in mode line
(column-number-mode 1)

;; disable GC when minibuffer is active
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 8000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; confirm to quit
(setq confirm-kill-emacs #'y-or-n-p)

;; Mac OSX specific settings
(if (eq system-type "darwin")
    (progn
      (use-package exec-path-from-shell
	:config (exec-path-from-shell-initialize))

      ;; use zsh installed from brew
      (setq explicit-shell-file-name "/usr/local/bin/zsh")
      (set-frame-font "Pragmata Pro 12" t t)
      (setq dired-use-ls-dired nil)))

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package ivy
  :diminish ivy-mode
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . swiper)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :demand)

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c C-f" . counsel-describe-function)
  ("C-c C-v" . counsel-describe-variable)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config (counsel-projectile-on))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package omnisharp
  :bind (("C-c o t" . omnisharp-go-to-definition)
	 ("C-c o o" . omnisharp-go-to-definition-other-window)
	 ("C-c o b" . pop-tag-mark))
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4))

  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  :demand)

(use-package web-mode
  :mode
  "\\.html?\\'"
  "\\.cshtml?\\'")

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
;;  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package gitignore-mode
  :config (add-hook 'gitignore-mode-hook (lambda ()
					   (setq require-final-newline t))))

(use-package solaire-mode
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)
;;  (add-hook 'after-revert-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg)
  :demand)

;; set default font
(set-face-attribute 'default nil :font (font-spec :family "Fira Mono" :size 11))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (gitignore-mode omnisharp company use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
