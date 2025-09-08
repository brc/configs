;;; core/packages.el --- Package Management Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core package management setup extracted from Spacemacs.
;; Handles package repositories, use-package installation, and configuration.

;;; Code:

;; Package Management Setup
;; Based on Spacemacs core/core-configuration-layer.el

;; Initialize package system
(require 'package)

;; Set package archives - using the same ones as Spacemacs
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Set package directory
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Initialize package system
(package-initialize)

;; Refresh package contents if we haven't done so
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Install use-package if not already present
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)

;; Configure use-package defaults
(setq use-package-always-ensure t    ; Always install packages if missing
      use-package-verbose t          ; Show loading info
      use-package-compute-statistics t) ; Enable statistics

;; Core Utility Packages
;; These are foundational libraries that multiple modules may depend on

(use-package dash
  :ensure t)

(use-package f
  :ensure t)

;;; core/packages.el ends here
