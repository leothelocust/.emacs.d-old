;;; _bindings.el --- Custom bindings

;; AUTHOR: Levi Olson <olson.levi@gmail.com>

;;; Commentary:

;; Custom bindings go here so as not to conflict with the main repo.

;;; Code:

(require 'company)
(require '_functions)

(add-hook 'comint-mode-hook (lambda () (local-set-key (kbd "C-l") 'clear-comint)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'restclient-response-received-hook #'my-restclient-hook)
(add-hook 'c-mode-common-hook 'c-setup)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(define-key custom-bindings-map (kbd "C-c D")       'define-word-at-point)
(define-key custom-bindings-map (kbd "C-@")         'er/expand-region)
(define-key custom-bindings-map (kbd "C-!")         'er/contract-region)
(define-key custom-bindings-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key                 (kbd "C->")         'mc/mark-next-like-this)
(global-set-key                 (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key                 (kbd "C-c C->")     'mc/mark-all-like-this)
(define-key custom-bindings-map (kbd "C-c m")       'magit-status)
(defvar company-active-map)
(defvar company-mode-map)
(define-key company-active-map (kbd "C-d")          'company-show-doc-buffer)
(define-key company-active-map (kbd "C-n")          'company-select-next)
(define-key company-active-map (kbd "C-p")          'company-select-previous)
(define-key company-active-map (kbd "<tab>")        'company-complete)
(define-key custom-bindings-map (kbd "C-c b")       'ivy-switch-buffer)
(define-key custom-bindings-map (kbd "C-c l")       'org-store-link)
(define-key custom-bindings-map (kbd "C-c t")       'org-set-tags)
(define-key custom-bindings-map (kbd "M-u")         'upcase-dwim)
(define-key custom-bindings-map (kbd "M-c")         'capitalize-dwim)
(define-key custom-bindings-map (kbd "M-l")         'downcase-dwim)
(define-key custom-bindings-map (kbd "M-o")         'other-window)
(define-key custom-bindings-map (kbd "C-c s")       'ispell-word)
(define-key custom-bindings-map (kbd "C-c C-d")     'org-capture)
(define-key custom-bindings-map (kbd "C-c <up>")    'windmove-up)
(define-key custom-bindings-map (kbd "C-c <down>")  'windmove-down)
(define-key custom-bindings-map (kbd "C-c <left>")  'windmove-left)
(define-key custom-bindings-map (kbd "C-c <right>") 'windmove-right)
(define-key custom-bindings-map (kbd "C-c a")
  (lambda () (interactive) (org-agenda nil "n")))

(define-key global-map          (kbd "M-p")         'jump-to-previous-like-this)
(define-key global-map          (kbd "M-n")         'jump-to-next-like-this)
(define-key custom-bindings-map (kbd "C-c e")       'find-user-init-file)
(define-key custom-bindings-map (kbd "C-x f")       'phil-columns)
(define-key custom-bindings-map (kbd "C-x k")       'kill-this-buffer-unless-scratch)
(define-key custom-bindings-map (kbd "C-c d")       'duplicate-thing)
(define-key custom-bindings-map (kbd "C-c c")       'comment-or-uncomment-region-or-line)
(define-key custom-bindings-map (kbd "C-o")         'new-line-below)
(define-key custom-bindings-map (kbd "C-S-o")       'new-line-above)
(define-key custom-bindings-map (kbd "<C-tab>")     'tidy)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings-map."
  t nil custom-bindings-map)

(define-key custom-bindings-map (kbd "M-q")         'kill-this-buffer)
(define-key custom-bindings-map (kbd "M-RET")       '(lambda () (interactive) (term (getenv "SHELL"))))

(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (concat "M-" (int-to-string n)))
                  (lambda () (interactive) (switch-shell n))))


(provide '_bindings)

;;; _bindings.el ends here
