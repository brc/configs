;;; init.el --- BRC's Custom Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal Emacs configuration built iteratively from Spacemacs features.
;; Each feature is added one at a time with understanding of what it does.

;;; Code:

;; Startup optimization - reduce garbage collection during startup
(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  ;; Use dark theme until we configure better theme support
  (load-theme 'wombat)

  ;; Load core modules
  (load (expand-file-name "core/packages" user-emacs-directory))
  (load (expand-file-name "core/key-bindings" user-emacs-directory))
  (load (expand-file-name "core/leader-key" user-emacs-directory))

  ;; Load feature modules (add more as we implement them)
  (load (expand-file-name "modules/which-key" user-emacs-directory))
  (load (expand-file-name "modules/evil" user-emacs-directory))
  (load (expand-file-name "modules/default-leader-keys" user-emacs-directory))
  (load (expand-file-name "modules/modeline" user-emacs-directory))
  (load (expand-file-name "modules/helm" user-emacs-directory))
  (load (expand-file-name "modules/projectile" user-emacs-directory))
  ;; (load (expand-file-name "modules/completion" user-emacs-directory))

  ;; Restore garbage collection settings after startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold 16777216  ; 16mb
                    gc-cons-percentage 0.1))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
 '(helm-minibuffer-history-key "M-p")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
