;;; _functions.el --- Custom functions.

;; AUTHOR: Levi Olson <olson.levi@gmail.com>

;;; Commentary:

;; Custom functions go here so as not to conflict with the main repo.

;;; Code:

(defun find-user-init-file ()
  "Edit the `config-org-file', in another window."
  (interactive)
  (find-file "/home/locust/.emacs.d/init.el"))

(defun jump-to-symbol-internal (&optional backwardp)
  "Jumps to the next symbol near the point if such a symbol exists.  If BACKWARDP is non-nil it jumps backward."
  (let* ((point (point))
         (bounds (find-tag-default-bounds))
         (beg (car bounds)) (end (cdr bounds))
         (str (isearch-symbol-regexp (find-tag-default)))
         (search (if backwardp 'search-backward-regexp
                   'search-forward-regexp)))
    (goto-char (if backwardp beg end))
    (funcall search str nil t)
    (cond ((<= beg (point) end) (goto-char point))
          (backwardp (forward-char (- point beg)))
          (t  (backward-char (- end point))))))

(defun jump-to-previous-like-this ()
  "Jumps to the previous occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal t))

(defun jump-to-next-like-this ()
  "Jumps to the next occurrence of the symbol at point."
  (interactive)
  (jump-to-symbol-internal))

(defun kill-this-buffer-unless-scratch ()
  "Works like `kill-this-buffer' unless the current buffer is the *scratch* buffer.  In which case the buffer content is deleted and the buffer is buried."
  (interactive)
  (if (not (string= (buffer-name) "*scratch*"))
      (kill-this-buffer)
    (delete-region (point-min) (point-max))
    (switch-to-buffer (other-buffer))
    (bury-buffer "*scratch*")))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun new-line-below ()
  "Create a new line below current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun new-line-above ()
  "Create a new line above current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1))

(defun duplicate-thing (comment)
  "Duplicates the current line, or the region if active.  If an argument (COMMENT) is given, the duplicated region will be commented out."
  (interactive "P")
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (point-at-bol)))
          (end   (if (region-active-p) (region-end) (point-at-eol))))
      (goto-char end)
      (unless (region-active-p)
        (newline))
      (insert (buffer-substring start end))
      (when comment (comment-region start end)))))

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

(defun phil-columns ()
  "Good 'ol Phil-Columns."
  (interactive)
  (message "Good 'ol fill-columns")
  (with-output-to-temp-buffer "*PHIL-COLUMN*"
    (shell-command "mpv --no-video 'https://www.youtube.com/watch?v=iIAuHqyYFkE&t=3m41s' > /dev/null 2>&1 & sleep 6; pkill mpv"))
  (other-window 1)
  (delete-window))

(declare-function first "Goto FIRST shell.")
(declare-function goto-non-shell-buffer "Goto something other than a shell buffer.")
(declare-function switch-shell "Switch shell.")

(let ((last-shell ""))
  (defun toggle-shell ()
    (interactive)
    (cond ((string-match-p "^\\*shell<[1-9][0-9]*>\\*$" (buffer-name))
           (goto-non-shell-buffer))
          ((get-buffer last-shell) (switch-to-buffer last-shell))
          (t (shell (setq last-shell "*shell<1>*")))))

  (defun switch-shell (n)
    (let ((buffer-name (format "*shell<%d>*" n)))
      (setq last-shell buffer-name)
      (cond ((get-buffer buffer-name)
             (switch-to-buffer buffer-name))
            (t (shell buffer-name)
               (rename-buffer buffer-name)))))

  (defun goto-non-shell-buffer ()
    (let* ((r "^\\*shell<[1-9][0-9]*>\\*$")
           (shell-buffer-p (lambda (b) (string-match-p r (buffer-name b))))
           (non-shells (cl-remove-if shell-buffer-p (buffer-list))))
      (when non-shells
        (switch-to-buffer (first non-shells))))))


(defadvice shell (after kill-with-no-query nil activate)
  "."
  (set-process-query-on-exit-flag (get-buffer-process ad-return-value) nil))

(declare-function comint-truncate-buffer ".")
(defun clear-comint ()
  "Run `comint-truncate-buffer' with the `comint-buffer-maximum-size' set to zero."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defvar my-restclient-access-token nil)
(defvar my-restclient-refresh-token nil)
(defun my-restclient-hook ()
  "Update token from a request."
  (save-excursion
    (save-match-data
      ;; update regexp to extract required data
      (when (re-search-forward "\"access_token\":\"\\(.*?\\)\"" nil t)
        (setq my-restclient-access-token (match-string 1)))
      (when (re-search-forward "\"refresh_token\":\"\\(.*?\\)\"" nil t)
        (setq my-restclient-refresh-token (match-string 1))))))

(defun c-setup ()
  "Compile."
  (local-set-key (kbd "C-c C-c") 'compile))


(provide '_functions)

;;; _functions.el ends here
