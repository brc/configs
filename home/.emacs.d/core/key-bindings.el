;;; core/key-bindings.el --- Global Key Bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Global key bindings extracted from .spacemacs configuration.
;; These are the core keybindings that should work everywhere.

;;; Code:

;; Core global bindings
;; Make C-h work as backspace (like in terminal)
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Move help command to M-h since C-h is now backspace
(global-set-key (kbd "M-h") 'help-command)

;; Shift+Insert for paste (useful in terminal environments)
(global-set-key (kbd "<S-insert>")
                (lambda ()
                  (interactive)
                  (if (display-graphic-p)
                      (x-selection-value)
                    (error "Shift+Insert paste not supported in terminal"))))

;; Unset M-t (was transpose-words) so we can use it as prefix
(global-unset-key (kbd "M-t"))

;; M-t : for eval-expression (handy for quick elisp evaluation)
(global-set-key (kbd "M-t :") 'eval-expression)

;; Note: Evil-specific bindings will be added when we implement Evil mode
;; For now, these are the core global bindings that work in any Emacs mode

;;; core/key-bindings.el ends here
