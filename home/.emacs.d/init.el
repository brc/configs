;;; init.el --- BRC's Custom Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Emacs configuration built iteratively from Spacemacs features.
;; Each feature is added one at a time with understanding of what it does.

;;; Code:

;; Startup optimization - reduce garbage collection during startup
(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

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

  ;; Restore garbage collection settings after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216  ; 16mb
                    gc-cons-percentage 0.1))))

;;; init.el ends here
