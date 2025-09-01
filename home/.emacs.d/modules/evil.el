;;; modules/evil.el --- Evil Mode Configuration -*- lexical-binding: t; -*-

;;; Phase 1: Minimal Evil configuration extracted from Spacemacs.
;;; Provides core vim functionality without the heavy customizations.

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


;;; Phase 2: Enhanced Evil configuration with essential packages.
;;; Provides comprehensive vim functionality with Spacemacs extensions.

;; evil-collection: Evilifies 100+ major modes
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; evil-surround: Change surrounds with cs"'
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; evil-nerd-commenter: Commenting with gc
(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
        ("gc" . evilnc-comment-operator)
        :map evil-visual-state-map
        ("gc" . evilnc-comment-operator)))

;; evil-numbers: Increment/decrement numbers with C-a/C-x
(use-package evil-numbers
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
        ("C-a" . evil-numbers/inc-at-pt)
        ("C-x" . evil-numbers/dec-at-pt)
        :map evil-visual-state-map
        ("C-a" . evil-numbers/inc-at-pt)
        ("C-x" . evil-numbers/dec-at-pt)))

;; evil-args: Argument text objects (da, ci,)
(use-package evil-args
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; evil-textobj-line: Line text objects (il, al)
(use-package evil-textobj-line
  :ensure t
  :after evil)

;; evil-indent-plus: Indent-based text objects
(use-package evil-indent-plus
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up))

;; evil-matchit: Enhanced % matching
(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; evil-anzu: Search highlighting
(use-package evil-anzu
  :ensure t
  :after evil
  :config
  (global-anzu-mode 1)
  (setq anzu-search-threshold 1000))

;; evil-visualstar: * in visual mode
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode))

;; evil-goggles: Visual feedback for operations
(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; evil-lion: Align with gl and gL
(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

;; evil-exchange: Exchange text with gx
(use-package evil-exchange
  :ensure t
  :after evil
  :config
  (evil-exchange-install))

;; evil-unimpaired: Bracket navigation ]b [b etc. (local implementation)
(with-eval-after-load 'evil
  (require 'dash)
  (require 'f)
  (load (expand-file-name "modules/evil-unimpaired" user-emacs-directory)))

;; vi-tilde-fringe: Visual line indicators
(use-package vi-tilde-fringe
  :ensure t
  :after evil
  :config
  (global-vi-tilde-fringe-mode))

;; evil-iedit-state: Multiple cursors with iedit
(use-package evil-iedit-state
  :ensure t
  :after evil
  :commands (evil-iedit-state/iedit-mode))

;; evil-cleverparens: Structural editing for Lisp
(use-package evil-cleverparens
  :ensure t
  :after evil
  :hook (emacs-lisp-mode . evil-cleverparens-mode))

;; evil-visual-mark-mode: Visual marks
(use-package evil-visual-mark-mode
  :ensure t
  :after evil
  :config
  (evil-visual-mark-mode))

;; evil-tutor: Learn Evil the hard way
(use-package evil-tutor
  :ensure t
  :after evil
  :commands (evil-tutor-start))

;; FIXME: Where does this go?
;; ;; hs-minor-mode: Code folding (built-in)
;; (add-hook 'prog-mode-hook 'hs-minor-mode)

;; eldoc: Show function signatures (built-in, enable globally)
(global-eldoc-mode 1)

(provide 'evil-config)
;;; modules/evil.el ends here
