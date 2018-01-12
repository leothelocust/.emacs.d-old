;;; _ivy.el --- ivy config

;; Author: Chris Bergquist

;;; Commentary:

;;; Code:

(require 'ivy-hydra)
(require 'ivy)
(require 'swiper)

(ivy-mode 1)
(counsel-mode)
(setq ivy-use-virtual-buffers t
      enable-recursive-minibuffers t
      ivy-height 25
      ivy-initial-inputs-alist nil
      ivy-extra-directories nil)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(defun ivy-open-current-typed-path ()
  (interactive)
  (when ivy--directory
    (let* ((dir ivy--directory)
           (text-typed ivy-text)
           (path (concat dir text-typed)))
      (delete-minibuffer-contents)
      (ivy--done path))))

(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-f") 'ivy-open-current-typed-path)

(provide '_ivy)

;;; _ivy.el ends here
