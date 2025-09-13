;;; modeline.el --- Modeline configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure doom-modeline for a fancy status line

;;; Code:

;; Add doom-modeline to package list
(add-to-list 'package-selected-packages 'doom-modeline)

;; Ensure package is installed
(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

;; Configure doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  ;; Show icons (requires all-the-icons package)
  (setq doom-modeline-icon t)
  ;; Show Evil state in modeline
  (setq doom-modeline-modal t)
  ;; Show buffer file name
  (setq doom-modeline-buffer-file-name-style 'auto)
  ;; Show line numbers
  (setq doom-modeline-buffer-state-icon t)
  ;; Height of the modeline
  (setq doom-modeline-height 25))

;;; modeline.el ends here
