;;; modules/helm.el --- Helm Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Essential Helm packages and configuration extracted from Spacemacs helm layer.
;; This provides the core Helm functionality without the full complexity of 47+ packages.

;;; Code:

;; =============================================================================
;; CORE HELM PACKAGES
;; =============================================================================

;; Install helm-core first (required dependency)
(use-package helm-core)

;; Main helm package
(use-package helm
  :after helm-core
  :init
  ;; Use helm by default for M-x, C-x C-f, and C-x b
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  :config
  ;; Enable helm mode for better integration
  (helm-mode 1)

  ;; Basic helm configuration
  (setq helm-split-window-inside-p t  ; open helm buffer inside current window
        helm-move-to-line-cycle-in-source t  ; move to end or beginning of source when reaching top or bottom
        helm-ff-search-library-in-sexp t  ; search for library in require and declare-function sexp
        helm-scroll-amount 8  ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t  ; Enable fuzzy matching for M-x
        helm-buffers-fuzzy-matching t  ; Enable fuzzy matching for buffers
        helm-recentf-fuzzy-match t))  ; Enable fuzzy matching for recent files

;; =============================================================================
;; ESSENTIAL HELM EXTENSIONS
;; =============================================================================

;; Use helm-ag fork from GitHub with better ripgrep support
(use-package helm-ag
  :vc (:url "https://github.com/brc/helm-ag"
            :rev "further-support-rg")
  :after helm
  :config
  (setq helm-ag-use-grep-ignore-list t)
  ;; Enable ripgrep support if available
  (when (executable-find "rg")
    (setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number --max-columns=150")))

;; helm-swoop for searching within buffers
(use-package helm-swoop
  :vc (:url "https://github.com/emacsattic/helm-swoop")
  :after helm
  :config
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically))

;; helm-themes for easy theme switching
;; NOTE: helm-themes (from older repositories) requires helm-config which was
;; removed in modern Helm versions. In older Helm, helm-config just loaded
;; helm-autoloads, which modern package.el/use-package handles automatically.
;; We provide a compatibility stub to satisfy the require statement.
(provide 'helm-config)

(use-package helm-themes
  :vc (:url "https://github.com/emacsattic/helm-themes")
  :after helm)

;; helm-projectile for project management
(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on))

;; helm-descbinds for better describe-bindings
(use-package helm-descbinds
  :after helm
  :init
  (setq helm-descbinds-window-style 'split
        helm-descbinds-disable-which-key nil)
  :config
  (add-hook 'helm-mode-hook 'helm-descbinds-mode))

;; ace-jump-helm-line for quick navigation within helm
(use-package ace-jump-helm-line
  :after helm
  :config
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-q") 'ace-jump-helm-line)))

;; helm-xref for better xref integration
(use-package helm-xref
  :after helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs
        xref-show-definitions-function 'helm-xref-show-defs))

;; wgrep for editing search results
(use-package wgrep
  :after helm
  :config
  (setq wgrep-auto-save-buffer t))

;; =============================================================================
;; SPACEMACS-STYLE FUNCTIONS
;; =============================================================================

(defun spacemacs/helm-M-x-fuzzy-matching ()
  "Helm M-x with fuzzy matching enabled"
  (interactive)
  (let ((completion-styles completion-styles))
    (add-to-list 'completion-styles 'flex t)
    (call-interactively 'helm-M-x)))

(defun spacemacs/helm-find-files ()
  "Custom helm-find-files with Spacemacs behavior."
  (interactive)
  (helm-find-files nil))

;; Search functions
(defun spacemacs/helm-project-smart-do-search ()
  "Search in current project using available search tools."
  (interactive)
  (cond
   ((executable-find "rg")
    (helm-do-ag (projectile-project-root)))
   ((executable-find "ag")
    (helm-do-ag (projectile-project-root)))
   (t
    (helm-do-grep-ag (projectile-project-root)))))

(defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
  "Search in current project using region or symbol at point."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (when input
      (setq helm-ag-insert-at-point 'symbol)
      (spacemacs/helm-project-smart-do-search))))

(defun spacemacs/helm-dir-do-ag ()
  "Search in current directory with `ag'."
  (interactive)
  (helm-do-ag default-directory))

(defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
  "Search with `ag' with a default input."
  (require 'helm-ag)
  (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;; make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing)
                (let ((res (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))
                             (this-fn thing))))
                  (when res (rxt-quote-pcre res))))))
    (funcall func dir)))

(defun spacemacs/helm-buffers-smart-do-search ()
  "Search in open buffers using available search tools."
  (interactive)
  (cond
   ((executable-find "rg")
    (helm-do-ag-buffers))
   ((executable-find "ag")
    (helm-do-ag-buffers))
   (t
    (helm-occur))))

(defun spacemacs/helm-jump-in-buffer ()
  "Jump in buffer using imenu facilities and helm."
  (interactive)
  (helm-semantic-or-imenu))

(defun spacemacs/helm-themes ()
  "Show helm themes without limit."
  (interactive)
  (let (helm-candidate-number-limit)
    (helm-themes)))

;; =============================================================================
;; LEADER KEY BINDINGS
;; =============================================================================

;; Replace existing placeholder bindings with helm versions
(spacemacs/set-leader-keys
  ;; Core Helm commands
  "SPC" 'spacemacs/helm-M-x-fuzzy-matching  ; SPC SPC for M-x

  ;; Buffer management
  "bb"  'helm-mini                          ; Buffer list
  "bU"  'helm-buffers-list                  ; Unfiltered buffer list

  ;; File management
  "ff"  'spacemacs/helm-find-files          ; Find files
  "fF"  'helm-find-files                    ; Raw helm-find-files
  "fL"  'helm-locate                        ; Locate files
  "fr"  'helm-recentf                       ; Recent files
  "fb"  'helm-filtered-bookmarks            ; Bookmarks

  ;; Search commands
  "/"   'spacemacs/helm-project-smart-do-search        ; Search in project
  "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol ; Search project with input
  "sb"  'spacemacs/helm-buffers-smart-do-search        ; Search in buffers
  "ss"  'helm-swoop                                    ; Search in current buffer
  "sS"  'helm-multi-swoop                              ; Search in multiple buffers
  "s C-s" 'helm-multi-swoop-all                        ; Search in all buffers
  "sj"  'spacemacs/helm-jump-in-buffer                 ; Jump in buffer (imenu)
  "sad" 'spacemacs/helm-dir-do-ag                     ; Search in current directory

  ;; Project commands (when projectile is available)
  "pb"  'helm-projectile-switch-to-buffer              ; Project buffers
  "pf"  'helm-projectile-find-file                     ; Project files
  "pd"  'helm-projectile-find-dir                      ; Project directories
  "pp"  'helm-projectile-switch-project                ; Switch projects
  "pr"  'helm-projectile-recentf                       ; Project recent files
  "ph"  'helm-projectile                               ; Project helm

  ;; Utilities
  "ry"  'helm-show-kill-ring                ; Kill ring
  "rl"  'helm-resume                        ; Resume last helm session
  "rs"  'helm-resume                        ; Resume last search
  "rm"  'helm-all-mark-rings               ; Mark rings
  "rr"  'helm-register                     ; Registers

  ;; Help and info
  "?"   'helm-descbinds                    ; Key bindings help
  "hda" 'helm-apropos                      ; Apropos
  "hi"  'helm-info-at-point               ; Info at point
  "hm"  'helm-man-woman                   ; Man pages

  ;; Themes
  "Ts"  'spacemacs/helm-themes             ; Themes

  ;; Colors
  "Cl"  'helm-colors                       ; Colors

  ;; Insert
  "iu"  'helm-ucs)                         ; Unicode characters

;;; modules/helm.el ends here
