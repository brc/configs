;;; modules/default-leader-keys.el --- Default Leader Key Bindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential leader key bindings extracted from Spacemacs core bootstrap
;; and spacemacs-defaults layers. This provides the most commonly used
;; bindings without the bloat of 696 total bindings.

;;; Code:

;; Note: leader-key.el is loaded before this file in init.el

;; =============================================================================
;; CORE BOOTSTRAP BINDINGS (from spacemacs-bootstrap layer)
;; =============================================================================

;; Evil/Register operations
(spacemacs/set-leader-keys
  "re" 'evil-show-registers
  "sc" 'spacemacs/evil-search-clear-highlight)

;; Help system
(spacemacs/set-leader-keys
  "hk" 'which-key-show-top-level)

;; =============================================================================
;; ESSENTIAL SPACEMACS-DEFAULTS BINDINGS
;; =============================================================================

;; Universal argument
(spacemacs/set-leader-keys "u" 'universal-argument)

;; Shell command
(spacemacs/set-leader-keys "!" 'shell-command)

;; Buffer operations - most essential ones
(spacemacs/set-leader-keys
  "TAB" 'spacemacs/alternate-buffer  ; Last buffer (SPC TAB)
  "bb"  'switch-to-buffer            ; Switch buffer
  "bd"  'spacemacs/kill-this-buffer  ; Delete current buffer
  "bn"  'next-buffer                 ; Next buffer
  "bp"  'previous-buffer             ; Previous buffer
  "bR"  'spacemacs/safe-revert-buffer ; Reload buffer
  "bs"  'spacemacs/switch-to-scratch-buffer) ; Scratch buffer

;; File operations - most essential ones
(spacemacs/set-leader-keys
  "ff"  'find-file                   ; Find file
  "fs"  'save-buffer                 ; Save file
  "fS"  'write-file                  ; Save as
  "fr"  'spacemacs/recentf           ; Recent files
  "fR"  'spacemacs/rename-current-buffer-file) ; Rename file

;; Window operations - basic ones
(spacemacs/set-leader-keys
  "wd"  'delete-window               ; Delete window
  "wh"  'evil-window-left            ; Move to left window
  "wj"  'evil-window-down            ; Move to bottom window
  "wk"  'evil-window-up              ; Move to top window
  "wl"  'evil-window-right           ; Move to right window
  "ws"  'split-window-below          ; Split horizontally
  "wv"  'split-window-right          ; Split vertically
  "ww"  'other-window                ; Switch windows
  "w/"  'split-window-right          ; Alternative split vertical
  "w-"  'split-window-below)         ; Alternative split horizontal

;; Quit operations
(spacemacs/set-leader-keys
  "qq"  'spacemacs/prompt-kill-emacs ; Quit Emacs
  "qr"  'spacemacs/restart-emacs-resume-layouts) ; Restart Emacs

;; Toggle operations - essential ones
(spacemacs/set-leader-keys
  "tn"  'display-line-numbers-mode   ; Toggle line numbers
  "tw"  'whitespace-mode             ; Toggle whitespace
  "tl"  'toggle-truncate-lines)      ; Toggle line truncation

;; =============================================================================
;; PLACEHOLDER FUNCTIONS
;; =============================================================================
;; These are functions that Spacemacs provides but we haven't implemented yet.
;; For now, we'll create simple placeholder functions or use built-in alternatives.

(defun spacemacs/alternate-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun spacemacs/kill-this-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun spacemacs/safe-revert-buffer ()
  "Revert buffer without confirmation if not modified."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(defun spacemacs/switch-to-scratch-buffer ()
  "Switch to *scratch* buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun spacemacs/recentf ()
  "Open recent files. Placeholder - will use helm/ivy later."
  (interactive)
  (if (fboundp 'recentf-open-files)
      (recentf-open-files)
    (message "Recent files not available - install recentf")))

(defun spacemacs/rename-current-buffer-file ()
  "Rename current buffer and its file if it exists."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (let ((new-name (read-file-name "New name: " filename)))
          (if (file-exists-p filename)
              (rename-file filename new-name 1))
          (set-visited-file-name new-name)
          (rename-buffer (file-name-nondirectory new-name)))
      (message "Buffer is not visiting a file"))))

(defun spacemacs/prompt-kill-emacs ()
  "Prompt before killing Emacs."
  (interactive)
  (if (yes-or-no-p "Really quit Emacs? ")
      (kill-emacs)))

(defun spacemacs/restart-emacs-resume-layouts ()
  "Restart Emacs (placeholder - layouts not implemented yet)."
  (interactive)
  (if (yes-or-no-p "Really restart Emacs? ")
      (progn
        (save-some-buffers)
        (kill-emacs 0))))

(defun spacemacs/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (evil-ex-nohighlight))

;; =============================================================================
;; DECLARE PREFIXES FOR WHICH-KEY
;; =============================================================================

(spacemacs/declare-prefix "b" "buffers")
(spacemacs/declare-prefix "f" "files")
(spacemacs/declare-prefix "h" "help")
(spacemacs/declare-prefix "q" "quit")
(spacemacs/declare-prefix "r" "registers/rings")
(spacemacs/declare-prefix "s" "search/symbol")
(spacemacs/declare-prefix "t" "toggles")
(spacemacs/declare-prefix "w" "windows")

;;; modules/default-leader-keys.el ends here
