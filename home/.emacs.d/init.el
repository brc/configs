;;; init.el --- BRC's Custom Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Emacs configuration built iteratively from Spacemacs features.
;; Each feature is added one at a time with understanding of what it does.

;;; Code:

;; Startup optimization - reduce garbage collection during startup
(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  ;; Load core modules
  (load (expand-file-name "core/packages" user-emacs-directory))
  (load (expand-file-name "core/key-bindings" user-emacs-directory))

  ;; Load feature modules (add more as we implement them)
  (load (expand-file-name "modules/which-key" user-emacs-directory))
  ;; (load (expand-file-name "modules/evil" user-emacs-directory))
  ;; (load (expand-file-name "modules/completion" user-emacs-directory))

  ;; Restore garbage collection settings after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216  ; 16mb
                    gc-cons-percentage 0.1))))

;;; init.el ends here
