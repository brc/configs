;;; modules/helm.el --- Helm Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential Helm packages and configuration extracted from Spacemacs helm layer.
;; This provides the core Helm functionality without the full complexity of 47+ packages.

;;; Code:

;; =============================================================================
;; CORE HELM PACKAGES
;; =============================================================================

;; Install helm-core first (required dependency)
(use-package helm-core)

;; Main helm package
(use-package helm
  :after helm-core
  :init
  ;; Use helm by default for M-x, C-x C-f, and C-x b
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  :config
  ;; Enable helm mode for better integration
  (helm-mode 1)

  ;; Basic helm configuration
  (setq helm-split-window-inside-p t  ; open helm buffer inside current window
        helm-move-to-line-cycle-in-source t  ; move to end or beginning of source when reaching top or bottom
        helm-ff-search-library-in-sexp t  ; search for library in require and declare-function sexp
        helm-scroll-amount 8  ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t  ; Enable fuzzy matching for M-x
        helm-buffers-fuzzy-matching t  ; Enable fuzzy matching for buffers
        helm-recentf-fuzzy-match t))  ; Enable fuzzy matching for recent files

;; =============================================================================
;; ESSENTIAL HELM EXTENSIONS (will be added in later modules)
;; =============================================================================

;; Note: helm-swoop, helm-projectile, etc. will be added in separate modules
;; to keep this core module focused on basic helm functionality

;; =============================================================================
;; SPACEMACS-STYLE FUNCTIONS
;; =============================================================================

(defun spacemacs/helm-M-x-fuzzy-matching ()
  "Helm M-x with fuzzy matching enabled"
  (interactive)
  (let ((completion-styles completion-styles))
    (add-to-list 'completion-styles 'flex t)
    (call-interactively 'helm-M-x)))

(defun spacemacs/helm-find-files ()
  "Custom helm-find-files with Spacemacs behavior."
  (interactive)
  (helm-find-files nil))

;; =============================================================================
;; LEADER KEY BINDINGS
;; =============================================================================

;; Replace existing placeholder bindings with helm versions
(spacemacs/set-leader-keys
  "SPC" 'spacemacs/helm-M-x-fuzzy-matching  ; SPC SPC for M-x
  "bb"  'helm-mini                          ; Buffer list (better than switch-to-buffer)
  "ff"  'spacemacs/helm-find-files          ; Find files
  "fr"  'helm-recentf                       ; Recent files (better than placeholder)
  "ry"  'helm-show-kill-ring                ; Kill ring
  "rl"  'helm-resume)                       ; Resume last helm session

;;; modules/helm.el ends here
