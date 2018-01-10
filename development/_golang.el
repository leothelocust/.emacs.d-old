;;; _golang.el --- Golang Config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'go-mode)
(require 'go-playground)
(require 'gorepl-mode)
(require 'company-go)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'gofmnt-before-save)
                           (local-set-key (kbd "M-.") 'godef-jump)
                           (local-set-key (kbd "M-,") 'pop-tag-mark)
                           (set (make-local-variable 'company-backends) '(company-go))
                           (setq company-tooltip-limit 20
                                 company-idle-delay .3
                                 company-echo-delay 0
                                 company-begin-commands '(self-insert-command))
                           (gorepl-mode)))
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/home/cbergquist/go")
(add-to-list 'exec-path "/home/cbergquist/go/bin")

(provide '_golang)

;;; _golang.el ends here
