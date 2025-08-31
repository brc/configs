;;; modules/evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Phase 1: Minimal Evil configuration extracted from Spacemacs.
;; Provides core vim functionality without the heavy customizations.

;;; Code:

(use-package evil
  :ensure t
  :init
  ;; Essential pre-configuration (must be set before evil loads)
  ;; From Spacemacs bootstrap: prepare for evil-collection
  (setq evil-want-keybinding nil)  ; Let evil-collection handle mode-specific bindings

  ;; Core evil preferences from Spacemacs core-spacemacs.el
  (setq evil-want-C-u-scroll t)    ; C-u scrolls up (vim behavior)

  :config
  ;; Enable evil mode globally
  (evil-mode 1)

  ;; Essential settings from Spacemacs bootstrap
  (setq evil-shift-width 4)        ; Default indent width

  ;; Keep region active when shifting (vim-like behavior)
  (evil-define-key 'visual 'global (kbd "<") "<gv")
  (evil-define-key 'visual 'global (kbd ">") ">gv")

  ;; Basic window management (from your .spacemacs)
  ;; C-w prefix for window operations (standard vim)
  ;; (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  ;; (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  ;; (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  ;; (define-key evil-window-map (kbd "<down>") 'evil-window-down)

  ;; Your custom binding from .spacemacs: C-i for xref-go-forward
  (define-key evil-normal-state-map (kbd "C-i") 'xref-go-forward)

  ;; Fix shift width after major mode changes
  (defun spacemacs//set-evil-shift-width ()
    "Set evil-shift-width based on major mode indent settings."
    (setq evil-shift-width (or (and (boundp 'tab-width) tab-width)
                               (and (boundp 'standard-indent) standard-indent)
                               4)))

  (add-hook 'after-change-major-mode-hook #'spacemacs//set-evil-shift-width))

;; Basic integration with help system
;; From Spacemacs: make help buffers more vim-like
(with-eval-after-load 'help-mode
  (evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button))

(provide 'evil-config)
;;; modules/evil.el ends here
