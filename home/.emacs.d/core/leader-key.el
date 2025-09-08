;;; modules/leader-key.el --- SPC Leader Key Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; SPC leader key implementation based on Spacemacs approach.
;; Uses bind-map to bind SPC in Evil modes to spacemacs-default-map.
;; Provides spacemacs/set-leader-keys function to add bindings.

;;; Code:

;; Install bind-map package
(use-package bind-map
  :ensure t)

;; Create the base keymap for all leader key commands
;; Based on spacemacs/core/core-keybindings.el:29-30
(defvar spacemacs-default-map (make-sparse-keymap)
  "Base keymap for all spacemacs leader key commands.")

;; Configuration variables (matching Spacemacs defaults)
(defvar dotspacemacs-leader-key "SPC"
  "The leader key for Evil normal and visual states.")

(defvar dotspacemacs-emacs-leader-key "M-m"
  "The leader key for Emacs state and Emacs mode.")

;; Set up bind-map to connect SPC to our keymap
;; Based on spacemacs-bootstrap/packages.el:64-69
(bind-map spacemacs-default-map
  :prefix-cmd spacemacs-cmds
  :keys (dotspacemacs-emacs-leader-key)
  :evil-keys (dotspacemacs-leader-key)
  :override-minor-modes t
  :override-mode-name spacemacs-leader-override-mode)

;; Function to add leader key bindings
;; Based on spacemacs/core/core-keybindings.el:76-95
(defun spacemacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(spacemacs/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key spacemacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

;; Add indentation function property
(put 'spacemacs/set-leader-keys 'lisp-indent-function 'defun)

;; Alias for compatibility
(defalias 'evil-leader/set-key 'spacemacs/set-leader-keys)

;; Declare prefix function for which-key integration
;; Based on spacemacs/core/core-keybindings.el:57-62
(defun spacemacs/declare-prefix (prefix name &rest more)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command."
  (declare (indent defun))
  (apply #'which-key-add-keymap-based-replacements spacemacs-default-map
         prefix name more))

;; Set up root prefix for which-key
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements spacemacs-default-map
    "" '("root" . "Spacemacs root")))

;;; modules/leader-key.el ends here
