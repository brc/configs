;;; modules/projectile.el --- Projectile Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Projectile configuration extracted from Spacemacs spacemacs-project layer.
;; Provides project management functionality including file navigation,
;; project switching, and project-wide operations.

;;; Code:

;; Core projectile functions from Spacemacs
;; Based on spacemacs/layers/+spacemacs/spacemacs-project/funcs.el

(defun spacemacs--projectile-directory-path ()
  "Retrieve the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (let ((directory-name (if (buffer-file-name)
                            (file-name-directory (buffer-file-name))
                          list-buffers-directory)))
    (when directory-name
      (file-relative-name
       (file-truename directory-name)
       (projectile-project-root)))))

(defun spacemacs--projectile-file-path ()
  "Retrieve the file path relative to project root.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when (buffer-file-name)
    (file-relative-name (file-truename (buffer-file-name)) (projectile-project-root))))

(defun spacemacs--projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (let ((file-path (spacemacs--projectile-file-path)))
    (when file-path
      (concat file-path ":" (number-to-string (line-number-at-pos))))))

(defun spacemacs--projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (let ((file-path (spacemacs--projectile-file-path-with-line)))
    (when file-path
      (format "%s:%s" file-path
              (+ (current-column) (if (boundp 'column-number-indicator-zero-based)
                                      (if column-number-indicator-zero-based 0 1)
                                    0))))))

(defun spacemacs/projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (let ((directory-path (spacemacs--projectile-directory-path)))
    (if directory-path
        (progn
          (kill-new directory-path)
          (message "%s" directory-path))
      (message "WARNING: Current buffer does not have a directory!"))))

(defun spacemacs/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (let ((file-path (spacemacs--projectile-file-path)))
    (if file-path
        (progn
          (kill-new file-path)
          (message "%s" file-path))
      (message "WARNING: Current buffer is not visiting a file!"))))

(defun spacemacs/projectile-copy-file-path-with-line ()
  "Copy and show the file path relative to project root, including line number."
  (interactive)
  (let ((file-path (spacemacs--projectile-file-path-with-line)))
    (if file-path
        (progn
          (kill-new file-path)
          (message "%s" file-path))
      (message "WARNING: Current buffer is not visiting a file!"))))

(defun spacemacs/projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (let ((file-path (spacemacs--projectile-file-path-with-line-column)))
    (if file-path
        (progn
          (kill-new file-path)
          (message "%s" file-path))
      (message "WARNING: Current buffer is not visiting a file!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile configuration
;; Based on spacemacs/layers/+spacemacs/spacemacs-project/packages.el

(use-package projectile
  :ensure t
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  ;; Core projectile settings
  (setq projectile-sort-order 'recentf
        projectile-cache-file (expand-file-name ".cache/projectile.cache"
                                                user-emacs-directory)
        projectile-known-projects-file (expand-file-name ".cache/projectile-bookmarks.eld"
                                                         user-emacs-directory)
        ;; Project search paths from .spacemacs configuration
        projectile-project-search-path '("/git" "/gi" "/gr" ("/gf" . 1) ("/gf/infra" . 1)
                                         "/gf/infra/terraform/cloudflare" "/gf/infra/terraform/vault"))

  :config
  (projectile-mode 1))

;; Leader key bindings - outside use-package to avoid compiler warnings
(spacemacs/set-leader-keys
  ;; File path
  "fyC" 'spacemacs/projectile-copy-file-path-with-line-column
  "fyD" 'spacemacs/projectile-copy-directory-path
  "fyL" 'spacemacs/projectile-copy-file-path-with-line
  "fyY" 'spacemacs/projectile-copy-file-path
  ;; Project
  "p!" 'projectile-run-shell-command-in-root
  "p&" 'projectile-run-async-shell-command-in-root
  "p%" 'projectile-replace-regexp
  "pa" 'projectile-toggle-between-implementation-and-test
  "pb" 'projectile-switch-to-buffer
  "pc" 'projectile-compile-project
  "pu" 'projectile-run-project
  "pd" 'projectile-find-dir
  "pD" 'projectile-dired
  "pe" 'projectile-edit-dir-locals
  "pf" 'projectile-find-file
  "pF" 'projectile-find-file-dwim
  "pE" 'projectile-find-references
  "pg" 'projectile-find-tag
  "pG" 'projectile-regenerate-tags
  "pi" 'projectile-install-project
  "pI" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "pp" 'projectile-switch-project
  "pr" 'projectile-recentf
  "pR" 'projectile-replace
  "pT" 'projectile-test-project
  "pv" 'projectile-vc)

;;; modules/projectile.el ends here
