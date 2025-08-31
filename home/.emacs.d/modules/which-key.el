;;; modules/which-key.el --- Which-key Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Which-key configuration extracted from Spacemacs.
;; Shows available key bindings in a popup buffer after a short delay.

;;; Code:

(use-package which-key
  :ensure t
  :config
  ;; Core settings - extracted from Spacemacs spacemacs-bootstrap/init-which-key
  (setq which-key-add-column-padding 1
        which-key-allow-multiple-replacements t
        which-key-echo-keystrokes 0.02
        which-key-idle-delay 0.4              ; Show after 0.4 seconds (Spacemacs default)
        which-key-idle-secondary-delay 0.01   ; Quick updates once shown
        which-key-max-description-length 32   ; Truncate long descriptions
        which-key-max-display-columns nil     ; Auto-calculate columns
        which-key-min-display-lines 6         ; Minimum popup height
        which-key-prevent-C-h-from-cycling t  ; Don't cycle with C-h
        which-key-sort-order 'which-key-prefix-then-key-order  ; Logical ordering
        which-key-sort-uppercase-first nil    ; Lowercase first
        which-key-special-keys nil            ; Don't highlight special keys
        which-key-use-C-h-for-paging t        ; Use C-h for help
        which-key-allow-evil-operators t)     ; Show operators in Evil mode

  ;; Simplify function names for cleaner display
  ;; Based on Spacemacs replacement rules but simplified
  (setq which-key-replacement-alist
        '(;; Remove common prefixes to clean up display
          (("\\`\\(.*\\) spacemacs/\\(.+\\)\\'" . "\\1 \\2"))
          (("\\`\\(.*\\) spacemacs//\\(.+\\)\\'" . "\\1 \\2"))
          (("\\`\\(.*\\) spacemacs-\\(.+\\)\\'" . "\\1 \\2"))

          ;; Group digit arguments together
          (("\\(.*\\)1 .. 9" . "digit-argument") . ("\\11..9" . "digit-argument"))
          (("\\(.*\\)C-0 .. C-5" . "digit-argument") . ("\\1C-0..5" . "digit-argument"))
          (("\\(.*\\)C-7 .. C-9" . "digit-argument") . ("\\1C-7..9" . "digit-argument"))))

  ;; Enable which-key mode globally
  (which-key-mode 1))


;; Optional: Add a simple toggle function (like Spacemacs does)
(defun brc/toggle-which-key ()
  "Toggle which-key mode on and off."
  (interactive)
  (which-key-mode 'toggle)
  (message "Which-key mode %s" (if which-key-mode "enabled" "disabled")))

;; You can bind this to a key if you want:
;; (global-set-key (kbd "C-c h k") #'brc/toggle-which-key)

;;; modules/which-key.el ends here
