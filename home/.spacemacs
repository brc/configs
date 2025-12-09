;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(graphviz
     nginx
     csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     asciidoc
     auto-completion
     docker
     (elfeed :variables
             elfeed-feeds '(("http://nullprogram.com/feed/" blog emacs)
                            ("https://about.gitlab.com/breaking-changes.xml" gitlab)))
     emacs-lisp
     ;; Delta caused too many issues, such as interpreting ANSI escape seqs
     ;; in diffs instead of showing the literal characters.  Disabling for now.
     ;;
     ;;     (git :variables
     ;;          git-enable-magit-delta-plugin t)
     git
     (go :variables go-tab-width 2)
                                        ;gtags
     helm
     html
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     javascript
     lsp
     markdown
     org
     pdf
     perl5
     php
     protobuf
     puppet
     (python :variables
             python-test-runner 'pytest)
     ranger
     restructuredtext
     (ruby :variables
           ruby-version-manager 'rbenv)
     ruby-on-rails
     ;; (shell :variables
     ;;       shell-default-height 30
     ;;       shell-default-position 'bottom)
     ;; (spacemacs-evil :variables
     ;;                 spacemacs-evil-collection-allowed-list
     ;;                 '(eww dired quickrun ztree))
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     sphinx
     sql
                                        ;syntax-checking  ;; enable flycheck
     systemd
     themes-megapack
     (treemacs :variables
               treemacs-use-git-mode 'extended)
     vimscript
     yaml
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     all-the-icons-nerd-fonts
     bash-completion
     claude-code-ide
     company-quickhelp
     company-terraform
     ;; coterm  ;; eat works 1,000x better
     ;; diff-hl
     dumb-jump
     eat  ;; Terminal emulator
     git-gutter
     groovy-mode
     hcl-mode
     helm-make  ;; I think this may already be included in Spacemacs...
     ini-mode
     lab  ;; GitLab support
     logview
     lsp-pyright
     magit-delta
     nerd-icons
     nerd-icons-completion
     nerd-icons-dired
     nerd-icons-ibuffer
     nerd-icons-xref
     ox-jira
     pacfiles-mode
     prometheus-mode
     rfc-mode
     sicp
     sqlite3
     strace-mode
     svg-lib  ;; Hopefully used for Material Design icons by LSP mode header line
     terraform-mode
     treesit-auto
     vterm
     yaml-pro
     ztree
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 19
                               :weight normal
                               :width normal)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   vim-style-remap-Y-to-y$ t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `ack' and `grep'.
   ;; (default '("rg" "ag" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-tree', `undo-fu' and `undo-redo', see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil

   ;; brett/duck/brc temp override while gnu.org is down today...
   ;; configuration-layer--elpa-archives
   ;; '(("melpa" . "melpa.org/packages/")
   ;;   ("org"   . "orgmode.org/elpa/")
   ;;   ("gnu"   . "/git/spacemacs-elpa-mirror-master/gnu"))
   ))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (setq goto-address-uri-schemes-ignored
        '("go:" "data:" "mailto:")))  ;; ignore Golang output like foo.go:123

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Automatically enable Treesitter flavored modes in Emacs 29.
  ;; See https://github.com/renzmann/treesit-auto?tab=readme-ov-file
  ;;     https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html
  ;;     https://www.reddit.com/r/emacs/comments/10iuim1/getting_emacs_29_to_automatically_use_treesitter/
  ;;
  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

  ;; Enable YAML-Pro minor mode for yaml-ts-mode
  (add-hook 'yaml-ts-mode-hook #'yaml-pro-ts-mode)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load custom files
  ;;
  ;;  See https://www.emacswiki.org/emacs/LoadingLispFiles and
  ;;      http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el
  ;;
  (add-to-list 'load-path "~/.elisp/enabled")

  (require 'vterm-anti-flicker-filter)

  ;; Jenkinsfile mode
  (require 'jenkinsfile-mode)
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("\\.jenkinsfile$" . jenkinsfile-mode))

  ;; Mustache mode
  ;; (require 'mustache-mode)  ;; Don't remember which file type I used this for...

  ;; Go Template mode
  (require 'go-template-mode)
  (add-to-list 'auto-mode-alist '("\\.tpl$" . go-template-mode))
  (add-hook 'go-template-mode-hook (lambda () (setq indent-tabs-mode nil)))

  ;; Arch PKGBUILDs
  (add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

  ;; Jinja2
  (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

  ;; $PAGER support in shell-mode (see https://github.com/mbriggs/emacs-pager)
  (require 'emacs-pager)
  (add-to-list 'auto-mode-alist '("\\.emacs-pager$" . emacs-pager-mode))
  (evil-define-key nil emacs-pager-mode-map (kbd "q") 'emacs-pager-kill-pager)

  ;; Ripgrep config for macOS
  (setenv "RIPGREP_CONFIG_PATH" (substitute-env-vars "${HOME}/.config/ripgrep/config"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PyRight LSP for Python
  ;;
  (use-package lsp-pyright
    :ensure t
    :custom (lsp-pyright-langserver-command "basedpyright") ;; or pyright
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp))))  ; or lsp-deferred

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; gopls LSP for Go
  ;;
  (setq go-backend 'lsp)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Claude Code
  ;;
  (use-package claude-code-ide
    ;;
    ;;   NB. Use local clone with edits instead of upstream latest
    ;;
    ;; :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
    ;;
    :load-path "/git/claude-code-ide.el"  ;; Both lines needed
    :requires (websocket web-server)      ;;

    :config
    (claude-code-ide-emacs-tools-setup)) ;; Optionally enable Emacs MCP tools
  ;; :bind ("C-c C-'" . claude-code-ide-menu) ;; Set your favorite keybinding


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Invert Mac keys: Command is Meta, Option is Super
  ;;
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Backspace using C-h
  ;;
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (evil-define-key nil helm-find-files-map (kbd "C-h") 'delete-backward-char)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get Help with M-h
  ;;
  (global-set-key (kbd "M-h") 'help-command)  ;; overrides (mark-paragraph)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Yank X11 PRIMARY selection (cut-buffer) using <Shift>-Insert
  ;;
  (global-set-key (kbd "<S-insert>")
                  #'(lambda () (interactive)(mouse-yank-primary (point))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Don't transpose words with M-t
  ;;
  ;;   Evil-Genius idea: Use it move around like tmux instead >:)
  ;;
  (global-unset-key (kbd "M-t"))

  ;; Make a keymap
  (defvar-keymap brc/tmux-mode-map
    :doc "Global Brett-tmux-style navigation bindings in Emacs."
    ;; "g t" #'eyebrowse-next-window-config
    ;; "g T" #'eyebrowse-prev-window-config
    "M-t M-p" #'persp-prev
    "M-t M-n" #'persp-next
    "M-t p" #'persp-prev
    "M-t n" #'persp-next
    "M-t h" #'evil-window-left
    "M-t l" #'evil-window-right
    "M-t j" #'evil-window-down
    "M-t k" #'evil-window-up
    "M-t <prior>" #'(lambda ()
                      (interactive)
                      (evil-scroll-page-up 1)
                      (evil-normal-state)))

  (define-minor-mode brc/tmux-mode
    "Global Brett-tmux navigation keybindings."
    :global t
    :keymap brc/tmux-mode-map)

  (brc/tmux-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mimic Vim <Ctrl-i> to move forward through the jump list
  ;;
  ;; FIXME This break org-mode because TAB is now wrong;
  ;;       do I really need to do this for only certain keymaps
  ;;       (like the travesty below)?  :-(
  (evil-define-key nil evil-normal-state-map (kbd "C-i") 'xref-go-forward)



  (global-set-key (kbd "M-t :") 'eval-expression)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Evil-mode overrides
  ;;
  ;; retain some Emacs movement bindings
  (evil-define-key nil evil-insert-state-map
    (kbd "C-a") nil  ;; unbind these from Evil insert mode
    (kbd "C-b") nil  ;;
    (kbd "C-d") nil  ;;
    (kbd "C-e") nil  ;;
    (kbd "C-f") nil  ;; (M-b, M-f, M-v, M-y, etc aren't shadowed as of 20240325)
    (kbd "C-n") nil  ;;
    (kbd "C-p") nil  ;;
    (kbd "C-v") nil  ;;
    (kbd "C-y") nil) ;;

  ;; don't record macros with "q" in Evil normal mode (too error prone)
  (evil-define-key nil evil-normal-state-map (kbd "q") nil)

  ;; use custom function for ffap with ace window
  (evil-define-key nil evil-normal-state-map (kbd "gA") 'brc/ffap-ace-window)

  ;; override `evil-repeat-pop-next' and `pop-tag-mark' bindings
  ;; (evil-define-key nil evil-normal-state-map (kbd "M-,") 'evil-scroll-left)
  ;; (evil-define-key nil evil-normal-state-map (kbd "M-.") 'evil-scroll-right)

  ;; override `evil-jump-to-tag' binding to use GNU Global
                                        ;(evil-define-key nil evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)

  ;; yank from X11 clipboard
  (evil-define-key nil evil-insert-state-map (kbd "M-v") 'clipboard-yank)

  ;; From Evil FAQ: don't update X11 PRIMARY selection with current visual region
  ;; brc: Looks like this should instead be done by setting
  ;;      `evil-visual-update-x-selection-p' to nil.
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; allow keybindings for magit-popup magic in docker mode
  (add-to-list 'evil-emacs-state-modes 'docker-container-mode)
  (add-to-list 'evil-emacs-state-modes 'ztree-mode)

  ;; overload `evil-lookup' motion to call `evil-lookup-func' interactively.
  ;; this allows for using `man' with `K' instead of `woman'.
  ;; see https://implementations-list.ourproject.narkive.com/utzxrdQu/evil-lookup-func-settings-in-evil-mode#post3
  (evil-define-motion evil-lookup ()
    "Look up the keyword at point interactively (brc).
Calls `evil-lookup-func'."
    (call-interactively evil-lookup-func))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spacemacs overrides
  ;;
  (spacemacs/set-leader-keys
    "#"  'spacemacs/alternate-buffer
    ;; "/"  'helm-projectile-rg
    "/"  'helm-projectile-ag
    "gg" 'magit-status
    "oc" 'claude-code-ide-menu
    "of" 'hs-toggle-hiding
    "oR" 'brc/sh-send-line-or-region-and-go
    "or" 'brc/sh-send-line-or-region
    "op" 'brc/sh-send-paragraph
    "oP" 'brc/fix-ps1-prompt-for-comint
    "os" 'sh-show-shell
    "rS" 'brc/resume-last-swoop-buffer
    "ss" 'helm-occur
    "w#" 'spacemacs/alternate-window
    "w_" 'brc/maximize-vertically
    "w|" 'spacemacs/maximize-horizontally)

  ;; ;; enable these keys for bash scripts
  ;; (spacemacs/set-leader-keys-for-major-mode 'sh-mode
  ;;   "oR" 'brc/sh-send-line-or-region-and-go
  ;;   "or" 'brc/sh-send-line-or-region
  ;;   "op" 'brc/sh-send-paragraph)

  ;;     ;; (...same, but with treesitter enabled)
  ;;   (spacemacs/set-leader-keys-for-major-mode 'bash-ts-mode
  ;;     "oR" 'brc/sh-send-line-or-region-and-go
  ;;     "or" 'brc/sh-send-line-or-region
  ;;     "op" 'brc/sh-send-paragraph)

  ;;     ;; (...same, but in Org mode)
  ;;   (spacemacs/set-leader-keys-for-major-mode 'org-mode
  ;;     "oR" 'brc/sh-send-line-or-region-and-go
  ;;     "or" 'brc/sh-send-line-or-region
  ;;     "op" 'brc/sh-send-paragraph)

  ;;   ;; (...same, but in Fundamental mode)
  ;;   (spacemacs/set-leader-keys-for-major-mode 'fundamental-mode
  ;;     "oR" 'brc/sh-send-line-or-region-and-go
  ;;     "or" 'brc/sh-send-line-or-region
  ;;     "op" 'brc/sh-send-paragraph)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit overrides
  ;;
  (with-eval-after-load 'transient (transient-bind-q-to-quit))
  (evil-define-key nil magit-status-mode-map (kbd "C-n") 'magit-section-forward)
  (evil-define-key nil magit-status-mode-map (kbd "C-p") 'magit-section-backward)
  ;; Don't use vim/spacemacs "ZZ" to save/quit in Magit status mode
  (evil-define-key nil magit-status-mode-map (kbd "Z") 'magit-worktree)
  (add-hook 'magit-process-mode-hook 'goto-address-mode)  ;; Clickable URLs
  ;; Disable line numbers for magit-delta
  ;;   see https://github.com/dandavison/magit-delta/issues/13
  ;;   (value for `magit-delta-delta-args' moved to Customize)

  ;; Cycle eyebrowse workspaces from Magit buffers
  (with-eval-after-load 'magit
    (dolist (kmap '(magit-status-mode-map        ;; git status buffer
                    magit-tag-section-map        ;;
                    magit-untracked-section-map  ;;
                    magit-file-section-map       ;;
                    magit-stash-section-map      ;;
                    magit-commit-section-map     ;;
                    magit-refs-mode-map               ;; git refs buffer
                    magit-remote-section-map          ;;
                    magit-branch-section-map          ;;
                    magit-shelved-branch-section-map  ;;
                    magit-hunk-section-map
                    magit-process-mode-map))
      (evil-define-key nil (symbol-value kmap)
        (kbd "g t") 'eyebrowse-next-window-config
        (kbd "g T") 'eyebrowse-prev-window-config)))

  ;; TODO: Make commit msg buffer wrap at 72 columns. The major mode is
  ;;       text-mode, so you will need to hook on the buffer name which is
  ;;       "COMMIT_EDITMSG"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GitLab configs
  ;;
  (setq lab-host (s-trim(f-read-text "/f/c/gitlab/url"))
        lab-token (s-trim(f-read-text "/f/c/gitlab/brc-token")))

  (define-key project-prefix-map "M" #'lab-list-project-merge-requests)
  (add-to-list 'project-switch-commands `(lab-list-project-merge-requests "List merge requests"))

  (define-key project-prefix-map "P" #'lab-list-project-pipelines)
  (add-to-list 'project-switch-commands `(lab-list-project-pipelines "List pipelines"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-mode overrides
  ;;
  ;; Always use Rifle for evil search
  ;; Update: Disabling this. Feature is available in Spacemacs via `SPC a o /'
                                        ;(evil-define-key 'normal org-mode-map (kbd "/") 'helm-org-rifle)
  ;;
  ;; TODO: Possibly implement this insanity instead:
  ;;       https://www.youtube.com/watch?v=a2jHqB1qWiY
  ;;
  ;;       ^ Major-mode keybindings in src blocks.
  ;;
  ;; I have defined the following overrides specifically because they bug me
  ;; while editing bash src blocks in org-mode, but they affect all of the
  ;; major mode, not just src blocks. What is implemented in the video above
  ;; would be required to only change bindings for src block regions.
  ;;
  ;; Override org-open-at-point because it wants to evaluate code blocks
  (evil-define-key nil org-mode-map (kbd "<return>") 'evil-ret)
  ;; Override evil-org-open-below because it pisses me off in bash src blocks
  (evil-define-key 'normal org-mode-map (kbd "o") 'evil-open-below)
  ;; Override org-metaleft because I expect M-h to show help!
  (evil-define-key nil org-mode-map (kbd "M-h") 'help-command)
  ;; Don't re-indent my code blocks! You're messing up my heredocs.
  (setq org-src-preserve-indentation t)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell-SCRIPT-mode (i.e., editing scripts)
  ;;
  (evil-define-key nil sh-mode-map
    (kbd "C-c C-e") 'brc/sh-send-line-or-region
    (kbd "C-c C-n") 'brc/sh-send-line-or-region-and-step)

  (add-hook 'bash-ts-mode-hook
            (lambda ()
              (setq-local xref-backend-functions
                          '(etags--xref-backend dumb-jump-xref-activate))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell-mode (comint process)
  ;;
  (autoload 'bash-completion-dynamic-complete "bash-completion"
    "Bash completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-mode-hook 'goto-address-mode)  ;; Clickable URLs

  ;; Make comint buffer output faster by using xterm-color-filter instead of
  ;; ansi-color-process-output
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  (with-eval-after-load 'ffap (add-to-list 'ffap-alist '(shell-mode . brc/shell-ffap)))
  (evil-define-key 'normal shell-mode-map (kbd "<return>") 'comint-send-input)
  (evil-define-key 'insert shell-mode-map
    (kbd "M-TAB") 'spacemacs/alternate-buffer
    (kbd "C-j") 'comint-send-input
    (kbd "C-k") 'kill-line
    (kbd "C-u") 'comint-kill-input
    (kbd "M-#") 'brc/comint-insert-comment ; XXX TODO
    (kbd "M-.") 'comint-insert-previous-argument
    (kbd "M-i a") 'brc/sh-awk-print
    (kbd "M-i c") "|count "
    (kbd "M-i g") "|grep -i "
    (kbd "M-g") "|grep -i "
    (kbd "M-i h") "|head "
    (kbd "M-h M-h") "--help"
    (kbd "M-i l") "|${PAGER} "
    (kbd "M-i q") "pacman -Q"
    (kbd "M-i s") 'brc/sh-sed-replace
    (kbd "M-i t") "|tail "
    (kbd "M-i w") 'brc/sh-while-read
    (kbd "M-i x") "|xargs "
    (kbd "C-N a") "-nathens "
    (kbd "C-N c") "-ncert-manager "
    (kbd "C-N h") "-nhelm-repo "
    (kbd "C-N g") "-ngitlab "
    (kbd "C-N G") "-ngitlab-build "
    (kbd "C-N i") "-nistio-system "
    (kbd "C-N I") "-nistio-operator "
    (kbd "C-N k") "-nkube-system "
    (kbd "C-N K") "-nkubeip "
    (kbd "C-N m") "-nmonitoring "
    (kbd "C-N p") "-nproduction "
    (kbd "C-N P") "-nproduction-testmode "
    (kbd "C-N s") "-nstaging "
    (kbd "C-N S") "-nstaging-testmode "
    (kbd "C-N v") "-nvault "
    (kbd "C-N C-N") "-n${NS} ")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; eshell overrides
  ;;
  (evil-define-key nil eshell-mode-map
    (kbd "M-r") 'helm-eshell-history
    (kbd "M-.") 'brc/eshell-yank-last-arg)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; inf-ruby overrides
  ;;
  (evil-define-key nil inf-ruby-minor-mode-map (kbd "C-c C-e") 'brc/ruby-send-line-or-region)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; kubernetes-el overrides
  ;;
  ;; These keys normally dispatch "logs" and "describe-mode" when you're looking
  ;; at an object -- in the case of looking at a ConfigMap, for instance, I
  ;; don't even think "logs" is useful; I'd rather have sane Evil motion state.
  ;;
  (evil-define-key 'motion kubernetes-display-thing-mode-map
    (kbd "l") 'evil-forward-char
    (kbd "h") 'evil-backward-char
    (kbd "E") 'evil-forward-WORD-end
    (kbd "n") 'evil-ex-search-next
    (kbd "?") 'evil-ex-search-backward)

  (evil-define-key 'motion kubernetes-logs-mode-map
    (kbd "l") 'evil-forward-char
    (kbd "h") 'evil-backward-char
    (kbd "E") 'evil-forward-WORD-end
    (kbd "n") 'evil-ex-search-next
    (kbd "?") 'evil-ex-search-backward)


  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; TRAMP wip
  ;;
  ;; (add-to-list 'tramp-methods
  ;;              '("kubectl"
  ;;                (tramp-login-program "kubectl")
  ;;                (tramp-login-args (("exec" "-it") ("-n" "%r") ("-c" "%p") ("%h") ("--") ("sh")))
  ;;                (tramp-remote-shell "/bin/sh")
  ;;                (tramp-remote-shell-args ("-i"))
  ;;                (tramp-connection-timeout 60)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Use Helm for Company completions
  ;;
  (autoload 'helm-company "helm-company")
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-M-i") 'helm-company)
       (define-key company-active-map (kbd "C-M-i") 'helm-company)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Man-mode
  ;;
  ;; override page width
  (setenv "MANWIDTH" "80")
  ;; navigate sections easily
  (evil-define-key nil Man-mode-map
    (kbd "M-n") 'Man-next-section
    (kbd "M-p") 'Man-previous-section
    (kbd "S")   'Man-goto-section)
  ;; jump to files easily
  (evil-define-key 'motion Man-mode-map
    (kbd "gf")  'find-file-at-point
    (kbd "gF")  'evil-find-file-at-point-with-line)
  (evil-global-set-key 'motion (kbd "K") 'man)
  ;; reuse current window for new Man buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Man .*\\*\\'" .
                 (display-buffer-reuse-mode-window
                  (inhibit-same-window . nil)
                  (mode . Man-mode))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Info-mode overrides for Evil
  ;;
  ;; use RET to follow nodes
  (evil-define-key nil Info-mode-map (kbd "<return>") 'Info-follow-nearest-node)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ztree overrides
  ;;
  ;; (evil-define-key nil ztree-mode-map (kbd "RET") 'ztree-perform-soft-action)
  ;; (evil-define-key nil ztree-mode-map (kbd "g r") 'ztree-refresh-buffer)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Terraform mode overrides for Evil
  ;;
  ;; use [[ and ]] to jump sections
  (evil-define-key 'normal terraform-mode-map (kbd "[[") 'hcl-beginning-of-defun)
  (evil-define-key 'normal terraform-mode-map (kbd "]]") 'hcl-end-of-defun)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Misc
  ;;
  (xterm-mouse-mode -1)  ;; disable mouse for terminal
  (define-key help-map (kbd "h") nil)  ;; unbind accidental hello-file (<Esc> hh)
  (add-hook 'find-file-hook 'brc/truncate-syslog-lines)  ;; don't wrap syslog

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; My defaults
  ;;
  (setq-default
   ;;create-lockfiles nil  ;; these broken symlinks wreak way too much havoc
   ;; (^ Actually: the broken symlinks are a nice indicator there is unsaved work :))
   evil-escape-key-sequence nil  ;; unbind default "fd" qwerty hack
   fci-rule-column 80  ;; vim `colorcolumn'
   ;; show more than 100 candidates
   helm-candidate-number-limit 500
   ;; this finds Makefile targets more correctly (using `make -nqp') than the
   ;; less-comprehensive regex in helm-make.el.
   helm-make-list-target-method 'qp
   projectile-project-search-path '("/git" "/gi" "/gr" ("/gf" . 2) ("/gf/infra" . 2) "/gf/infra/terraform" "/gf/spedn/service" ("/gf/common" . 3) ("/gf/spend" . 3))
   org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
   org-todo-keyword-faces '(("INPROGRESS" . "yellow"))
   ranger-cleanup-on-disable nil ;; Don't auto-kill Deer buffers
   scroll-margin 5     ;; vim `scrolloff'
   ;; (emacs 26.1 changed `term-char-mode' to disallow cursor movement in evil's
   ;;  normal mode--restore the previous behavior)
   term-char-mode-point-at-process-mark nil
   ;; treesit-language-source-alist
   ;;   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   ;;     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   ;;     (go "https://github.com/tree-sitter/tree-sitter-go")
   ;;     (json "https://github.com/tree-sitter/tree-sitter-json")
   ;;     (make "https://github.com/alemuller/tree-sitter-make")
   ;;     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   ;;     (python "https://github.com/tree-sitter/tree-sitter-python"))

   ;; draw vundo tree with UTF-8 symbols instead of ASCII chars
   ;; (requires a font that supports it!)
   vundo-glyph-alist vundo-unicode-symbols

   ;; Make long lines render more quickly by disabling bidirectional languages
   bidi-inhibit-bpa t
   bidi-paragraph-direction 'left-to-right
   ) ;setq-default

  ) ;dotspacemacs/user-config()

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
 '(mouse-yank-at-point t)
 '(package-selected-packages
   '(company-quickhelp company-terraform terraform-mode hcl-mode ranger sicp yasnippet-classic-snippets zones go-guru go-eldoc flycheck-pos-tip pos-tip flycheck company-go go-mode phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode web-beautify livid-mode skewer-mode simple-httpd js2-refactor multiple-cursors js2-mode js-doc coffee-mode flyspell-correct-helm flyspell-correct auto-dictionary arch-packer dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat groovy-mode helm-gtags ggtags strace-mode ini-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data puppet-mode yaml-mode mmm-mode markdown-toc markdown-mode jinja2-mode gh-md company-ansible ansible-doc ansible xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help vimrc-mode dactyl-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme lv rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit transient git-commit with-editor helm-company company-statistics helm-c-yasnippet fuzzy company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(safe-local-variable-values
   '((encoding . utf-8)
     (eval ansible 1)
     (eval add-to-list 'company-backends 'company-ansible))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "magenta" :foreground "brightyellow" :underline t))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#444444" :foreground "#87D700"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#FFDEC8" :foreground "red")))))

;; from https://www.reddit.com/r/emacs/comments/r3dzcw/findfileatpoint_in_acewindow_selected_window/
(defun brc/ffap-ace-window ()
  "Like `ffap-other-window', but window selected via ace window."
  (interactive)
  (let ((filename (thing-at-point 'filename)))
    (if (> (length (aw-window-list)) 1)
        (progn
          (aw-switch-to-window (aw-select nil))
          (find-file-at-point (ffap-prompter filename)))
      (find-file-at-point (ffap-prompter filename)))))

(defun brc/shell-ffap (name)
  "My ffap-file-at-point helper for shell-mode major mode."
  (concat "/tmp/" name))
;;
;; TODO Have a list of paths to try and iterate each one until found.
;;      See (ffap-locate-file) for reference.
;;
                                        ;(let ((paths-to-try '("/tmp"))
                                        ;      path found))

;; https://tsdh.wordpress.com/2007/03/
(defun brc/maximize-vertically ()
  "Delete all windows above or below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))


;; modeled from `resume-last-search-buffer' in ~/.emacs.d/layers/+completion/helm/funcs.el
(defun brc/resume-last-swoop-buffer ()
  "Open last helm-swoop buffer."
  (interactive)
  (cond ((get-buffer "*Helm Swoop*")
         (helm-resume "*Helm Swoop*"))
        (t
         (message "No previous Swoop buffer found"))))

;; Modeled after `sh-send-line-or-region-and-step' in
;;   /usr/share/emacs/<version>/lisp/progmodes/sh-script.el.gz
;;
(defun brc/sh-send-command (stay)
  "Send the current line to the inferior shell.
When region is active, send region instead.
When STAY is non-nil, keep point where it currently is, otherwise move point to shell process buffer after sending text."
  ;; FIXME `sh-send-txt' in `shell-script.el' is causing issues sending a
  ;;       *multiline* region when the region includes any command that needs to
  ;;       read stdin from the user.  Sending a single entire line or typing the
  ;;       line manually to comint (in shell-mode) doesn't exhibit the behavior.
  ;;       The problem is simply caused by the concatenation of the text with a
  ;;       newline character `\n'.  When sending a region that is selected line-
  ;;       wise (i.e., S-v in Evil), the appended newline will appear as user
  ;;       input instead of acting to submit the text to comint; this only
  ;;       occurs when a multiline region contains *entire lines* (i.e., not
  ;;       when a multiline region is selected character-wise and contains only
  ;;       part of a line at the beginning or end). `comint.el' has two methods
  ;;       for sending text: `comint-send-string' and `comint-send-region'
  ;;       (these are just wrappers around `process-send-string' and
  ;;       `process-send-region'). `sh-send-text' only ever uses the former,
  ;;       which always concatenates a newline to the text. Currently, the
  ;;       solution is to detect whether we're sending a multiline region or not
  ;;       and conditionally use `comint-send-region'.
  ;;       This is all tested with Evil in visual select mode; haven't tried
  ;;       with mark and point in vanilla Emacs. Fix this upstream in
  ;;       `shell-script.el'.
  ;;
  ;; TODO Investigate `sh-execute-region' in `sh-script.el'.
  ;; TODO Is it possible to use high-level `comint-send-input' interface instead
  ;;      low-level `comint-send-string' so that the shell buffer can be aware
  ;;      of the injected input?  That way, `C-c C-p' and `C-c C-o' would work.
  ;;
  (let ((src-window (get-buffer-window))
        (shell-buffer (process-buffer (sh-shell-process t))))
    (if (use-region-p)
        (let ((numlines (count-lines (region-beginning) (region-end)))
              (from (region-beginning))
              (to (region-end)))
          (if (eq numlines 1)
              (sh-send-text (buffer-substring-no-properties from to))
            (comint-send-region (sh-shell-process t) from to)))
      (let ((from (line-beginning-position))
            (to (line-end-position)))
        (sh-send-text (buffer-substring-no-properties from to))))
    ;; TODO Make sure shell buffer is visible in the last frame where it was
    ;;      showing, possibly creating a new window if the buffer is not
    ;;      currently visible. See `(display-buffer)' help for tons of options.
    ;; (display-buffer shell-buffer)  ;; This shows buffer in current frame
    (unless stay (pop-to-buffer shell-buffer))))

(defun brc/sh-send-line-or-region ()
  "Send the current line to the inferior shell; remain in current code window.
When region is active, send region instead."
  (interactive)
  (brc/sh-send-command t))

(defun brc/sh-send-line-or-region-and-go ()
  "Send the current line to the inferior shell and move there.
When region is active, send region instead."
  (interactive)
  (brc/sh-send-command nil))

(defalias 'brc/sh-send-paragraph
  ;; Select paragraph (v i p),
  ;; send region (SPC o r),
  ;; deselect (ESC),
  ;; move point to first column (0)
  (kmacro "v i p SPC o r <escape> 0"))

;; TODO re-implement per changes to `brc/sh-send-command' above
;;
;; (defun brc/sh-send-line-or-region-and-step ()
;;   "Send the current line to the inferior shell and move point to next line;
;; remain in current code window. When region is active, send region instead."
;;   (interactive)
;;   (brc/sh-send-command nil)
;;   (if (use-region-p)
;;       (goto-char (region-end))
;;     (goto-char (1+ (line-end-position)))))

(defun brc/sh-awk-print ()
  "Inject '|awk {print $}' and move point inside braces."
  (interactive)
  (insert "|awk '{print $}'")
  (backward-char 2))

(defun brc/sh-sed-replace ()
  "Inject '|sed s///' and move point inside slashes."
  (interactive)
  (insert "|sed -r -e 's///'")
  (backward-char 3))

(defun brc/sh-while-read ()
  "Inject '|while read x' and move point inside loop body."
  (interactive)
  (insert "|while read x; do ; done")
  (backward-char 2))

(defun brc/ruby-send-line-or-region ()
  "Send the current line or active region to the inferior Ruby process."
  (interactive)
  ;; FIXME region doesn't work
  (if (use-region-p)
      (ruby-send-region)
    (ruby-send-line)))


(defun brc/truncate-syslog-lines ()
  "Toggle line truncation on when file name ends in `.log'"
  (if (string-suffix-p ".log" (buffer-name))
      (toggle-truncate-lines 1)))

;; From https://emacs.stackexchange.com/questions/41222/how-can-i-pass-the-no-line-break-argument-to-base64-encode-region-in-m-x
;;
(defun brc/base64-encode-region-prefix-arg (&rest _args)
  "Pass prefix arg as third arg to `base64-encode-region'."
  (interactive "r\nP"))

(advice-add 'base64-encode-region :before #'brc/base64-encode-region-prefix-arg)


(defun brc/replace-ec2-hostname-with-ip ()
  "Replace all occurrences of `ip-w-x-y-z.ec2.internal' with `w.x.y.z' on
current line"
  (interactive "*")  ;; fail if buffer readonly
  ;; TODO Add region support
  ;;
  ;;      See https://stackoverflow.com/a/58308604/2228206
  ;;          http://xahlee.info/emacs/emacs/elisp_command_working_on_string_or_region.html
  ;;
  ;;      Either make a separate function that works on a region and
  ;;      conditionally call it from this function, or make a monolithic
  ;;      function that handles all cases.
  ;;
  ;;      The bitch of this for implementing region support was figuring out why
  ;;      BOUND wasn't being honored in the call to `(re-search-forward)'; it
  ;;      turns out that shortening the size of the buffer (by changing it)
  ;;      causes BOUND to now be farther out than we want it to be; see the
  ;;      comment in the same SO post:
  ;;
  ;;        https://stackoverflow.com/a/58322478/2228206
  ;;
  ;;         ^ "Unfortunately `(use-region-p)' also has some quirks. BOUND needs
  ;;            to be updated when a replacement will change the length of the
  ;;            region."
  ;;
  ;; Hacky WIP
                                        ; (let (start end)
                                        ;   (if (use-region-p)
                                        ;       (progn
                                        ;         (setq start (region-beginning)
                                        ;               end (region-end))
                                        ;         ;; Linewise visual mode is selecting the newline and placing
                                        ;         ;; my cursor on the following line (outside of region)
                                        ;         ;; Detect this condition and reduce `end'
                                        ;         (when (char-equal ?\C-j (char-after (- end 1)))
                                        ;           (setq end (- end 2))
                                        ;           (message "Found newline!")
                                        ;           (message "Start is %d" start)
                                        ;           (message "End is %d" end)))
                                        ;     (setq start (line-beginning-position)
                                        ;           end (line-end-position)))
                                        ;  (goto-char start)
  (save-excursion
    (beginning-of-line)
    (let ((re "ip-\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\..+\\.internal")
          (end (make-marker)))
      (set-marker end (line-end-position))
      (while (re-search-forward re end t)
        (replace-match "\\1.\\2.\\3.\\4"))
      (set-marker end nil))))


(defun brc/replace-epoch-with-readable-date ()
  "Replace all occurrences of Unix epoch timestamps on the current line with
respective human-readable dates. For example, replace `1714834042' with
\"Sat May 4 07:47:22 AM PDT 2024\""
  (interactive "*")  ;; fail if buffer readonly
  (save-excursion
    (beginning-of-line)
    (let ((re "\\b[0-9]\\{7,10\\}\\b")
          (end (make-marker)))
      (set-marker end (line-end-position))
      (while (re-search-forward re end t)
        (replace-match
         (string-chop-newline
          (shell-command-to-string
           (concat "date --date=@"
                   (string-chop-newline
                    (number-to-string (number-at-point))))))))
      (set-marker end nil))))


(defun brc/make-buffer-plain-text ()
  "Remove all text properties from entire buffer. Useful for yanking output
from comint buffer and pasting it somewhere else so you can edit it. See
https://emacs.stackexchange.com/questions/12526/how-to-remove-all-text-properties-in-a-buffer"
  (interactive "*")  ;; fail if buffer readonly
  ;; TODO: Operate on region instead of entire buffer
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell functions
;;
(defun eshell/kls ()
  "List Kubernetes clusters."
  (with-temp-buffer
    (cd "/dr/k8s/kubeconfigs")
    (shell-command
     (concat "ls kubeconfig-*-sa"
             "|sed -e s/kubeconfig-// -e 's/-sa$//'"
             "|sort -u")
     (buffer-name))
    (buffer-string)))

;; TODO: Figure out how to cycle thru preceding commands and swap word upon
;;       subsequent presses of M-.
(defun brc/eshell-yank-last-arg (n)
  "Mimic Bash `yank-last-arg'."
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unimpaired-paste emulator (YOPO)
;;
;;  TODO: seems like newline-and-indent not being restored
;;
;; ;; https://github.com/tpope/vim-unimpaired/blob/master/plugin/unimpaired.vim
;; (defun setup_paste ()
;;   (setq-local oldret (key-binding "\C-m"))
;;   (local-set-key "\C-m" 'evil-ret)  ;; was 'newline-and-indent)
;;   (add-hook 'evil-insert-state-exit-hook
;;             #'(lambda () (when (boundp 'oldret)
;;                           (local-set-key "\C-m" oldret)
;;                           (makunbound 'oldret)))
;;             nil t))

;; XXX Brokenness with `helm-ag' and ripgrep.
;;
;;     This has no affect:
;;       (custom-set-variables
;;         '(helm-ag-base-command
;;           "rg --no-config --no-heading --no-ignore --color=never \
;;            --line-number --smart-case --hidden --follow --glob=!.git/**"))
;;
;;     The `helm-ag-base-command' variable fundamentally doesn't work with
;;     `spacemacs/helm-files-do-rg' (the default function used by
;;     `spacemacs/helm-project-smart-do-search' when `rg' is installed).
;;
;;     `spacemacs/helm-files-do-rg' is simply a wrapper that calls `helm-do-ag'
;;      (see: /gi/configs/home/spacemacs/layers/+completion/helm/funcs.el).
;;
;;     The plethora of `--ignore=foo' options below don't even make sense for rg.
;;     These default options are defined by `grep.el' (see
;;     /usr/share/emacs/28.2/lisp/progmodes/grep.el.gz) because `helm-ag' uses
;;     the variables from that code in `helm-ag--grep-ignore-list-to-options'.
;;
;;     When I became suspicous that my `helm-ag-base-command' settings
;;     weren't being honored, I first snooped the process table and found:
;;
;; /usr/bin/rg --smart-case --no-heading --color=never --line-number
;;   --max-columns=512 --ignore=.#* --ignore=*.o --ignore=*~ --ignore=*.bin
;;   --ignore=*.lbin --ignore=*.so --ignore=*.a --ignore=*.ln --ignore=*.blg
;;   --ignore=*.bbl --ignore=*.elc --ignore=*.lof --ignore=*.glo --ignore=*.idx
;;   --ignore=*.lot --ignore=*.fmt --ignore=*.tfm --ignore=*.class --ignore=*.fas
;;   --ignore=*.lib --ignore=*.mem --ignore=*.x86f --ignore=*.sparcf
;;   --ignore=*.dfsl --ignore=*.pfsl --ignore=*.d64fsl --ignore=*.p64fsl
;;   --ignore=*.lx64fsl --ignore=*.lx32fsl --ignore=*.dx64fsl --ignore=*.dx32fsl
;;   --ignore=*.fx64fsl --ignore=*.fx32fsl --ignore=*.sx64fsl --ignore=*.sx32fsl
;;   --ignore=*.wx64fsl --ignore=*.wx32fsl --ignore=*.fasl --ignore=*.ufsl
;;   --ignore=*.fsl --ignore=*.dxl --ignore=*.lo --ignore=*.la --ignore=*.gmo
;;   --ignore=*.mo --ignore=*.toc --ignore=*.aux --ignore=*.cp --ignore=*.fn
;;   --ignore=*.ky --ignore=*.pg --ignore=*.tp --ignore=*.vr --ignore=*.cps
;;   --ignore=*.fns --ignore=*.kys --ignore=*.pgs --ignore=*.tps --ignore=*.vrs
;;   --ignore=*.pyc --ignore=*.pyo --ignore=SCCS --ignore=RCS --ignore=CVS
;;   --ignore=MCVS --ignore=.src --ignore=.svn --ignore=.git --ignore=.hg
;;   --ignore=.bzr --ignore=_MTN --ignore=_darcs --ignore={arch} 'my.*search'
;;
;; This confirmed my suspicion.


;; From https://xenodium.com/emacs-clone-git-repo-from-clipboard/
;; Also see Reddit post: https://www.reddit.com/r/emacs/comments/k3iter/simplequick_git_repo_clone_from_browser_to_emacs/
;;
(defun brc/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "/git/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc
                          (lambda (process state)
                            (let ((output (with-current-buffer (process-buffer process)
                                            (buffer-string))))
                              (kill-buffer (process-buffer process))
                              (if (= (process-exit-status process) 0)
                                  (progn
                                    (message "finished: %s" command)
                                    (dired project-dir))
                                (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(defun brc/fix-ps1-prompt-for-comint ()
  "Set new $PS1 value for the bash process being used by comint."
  ;; TODO Check the current major mode first before doing anything?
  (interactive)
  (evil-insert 0)
  ;; FIXME Kill any existing input text first.
  ;;       (comint-send-input) will send whatever input already exists on the
  ;;       line... maybe you can find out whether you actually killed any text
  ;;       and then conditionally restore it after the comint-send-input.
  (comint-kill-input)
  (insert "unset PROMPT_COMMAND; ")
  (insert "export PS1='[\\[\\e[1;32m\\]\\h\\[\\e[0m\\]:\\[\\e[1;34m\\]\\w\\[\\e[0m\\]]$ '")
  (comint-send-input)
  (move-beginning-of-line))

(defun brc/update-system-path ()
  "Update Emacs' $PATH from the shell."
  (interactive)
  (let ((path-string (shell-command-to-string "bash -l -c 'echo $PATH'")))
    (setenv "PATH" (car (split-string path-string "\n")))
    (setq exec-path (split-string (getenv "PATH") path-separator))))

;; (defun brc/go-mode-hook ()
;;	"Custom settings for go-mode."
;;	;; Use tabs for indentation
;;	(setq-default indent-tabs-mode t)
;;	;; Set the visual width of a tab character to 2 spaces
;;	(setq-default tab-width 2)
;;	;; Set the evil shift width to 4 (which will be a single tab due to indent-tabs-mode t)
;;	(setq evil-shift-width 1)
;;	;; If using go-ts-mode (tree-sitter mode for Go), set its specific offset
;;	(when (boundp 'go-ts-mode-indent-offset)
;;		(setq go-ts-mode-indent-offset 1)))

;; (add-hook 'go-ts-mode-hook 'brc/go-mode-hook)

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(Man-notify-method 'aggressive)
   '(comint-process-echoes t)
   '(comint-scroll-to-bottom-on-input t)
   '(emacs-pager-max-line-coloring 5000)
   '(evil-lookup-func 'man)
   '(evil-want-Y-yank-to-eol t)
   '(explicit-shell-file-name "bash")
   '(helm-buffer-max-length nil)
   '(helm-completion-style 'helm)
   '(js-indent-level 2)
   '(kubernetes-pod-restart-warning-threshold 2)
   '(kubernetes-poll-frequency 300)
   '(kubernetes-redraw-frequency 5)
   '(logview-additional-submodes
     '(("brc/Spot.io-Ocean" (format . "TIMESTAMP LEVEL MESSAGE") (levels . "SLF4J")
        (timestamp "ISO 8601 datetime (with 'T') + millis - UTC")
        (aliases "ocean"))))
   '(logview-additional-timestamp-formats
     '(("ISO 8601 datetime (with 'T') + millis - UTC"
        (java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
        (datetime-options :any-decimal-separator t))))
   '(magit-blame-echo-style 'margin)
   '(magit-delta-delta-args
     '("--max-line-distance" "0.6" "--true-color" "always" "--color-only"
       "--features" "magit"))
   '(magit-diff-refine-hunk t)
   '(magit-diff-refine-ignore-whitespace nil)
   '(magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
   '(mouse-yank-at-point t)
   '(org-export-backends '(ascii html icalendar latex man md odt))
   '(org-fold-core-style 'overlays)
   '(package-selected-packages
     '(ac-ispell ace-jump-helm-line ace-link ace-window adaptive-wrap afternoon-theme
                 aggressive-indent alect-themes alert all-the-icons-nerd-fonts
                 ample-theme ample-zen-theme anaconda-mode ansible ansible-doc
                 anti-zenburn-theme anzu apropospriate-theme arch-packer async
                 auto-compile auto-complete auto-dictionary auto-highlight-symbol
                 auto-yasnippet autothemer avy badwolf-theme bash-completion
                 bind-key bind-map birds-of-paradise-plus-theme bubbleberry-theme
                 bundler busybee-theme cherry-blossom-theme chruby claude-code-ide
                 clean-aindent-mode clues-theme coffee-mode
                 color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow
                 column-enforce-mode company company-anaconda company-ansible
                 company-go company-quickhelp company-statistics company-terraform
                 company-web cyberpunk-theme cython-mode dactyl-mode dakrone-theme
                 darkburn-theme darkmine-theme darkokai-theme darktooth-theme dash
                 dash-functional define-word diminish django-theme docker
                 docker-tramp dockerfile-mode dracula-theme drupal-mode dumb-jump
                 elisp-slime-nav emmet-mode epl esh-help eshell-prompt-extras
                 eshell-z espresso-theme eval-sexp-fu evil evil-anzu evil-args
                 evil-ediff evil-escape evil-exchange evil-iedit-state
                 evil-indent-plus evil-lisp-state evil-magit evil-matchit evil-mc
                 evil-nerd-commenter evil-numbers evil-search-highlight-persist
                 evil-surround evil-tutor evil-unimpaired evil-visual-mark-mode
                 evil-visualstar exec-path-from-shell exotica-theme expand-region
                 eyebrowse f fancy-battery farmhouse-theme fill-column-indicator
                 flatland-theme flatui-theme flx flx-ido flycheck flycheck-pos-tip
                 flyspell-correct flyspell-correct-helm fuzzy gandalf-theme ggtags
                 gh-md git-commit git-link git-messenger git-timemachine
                 gitattributes-mode gitconfig-mode gitignore-mode gntp gnuplot
                 go-eldoc go-guru go-mode golden-ratio google-translate
                 gotham-theme goto-chg grandshell-theme groovy-mode
                 gruber-darker-theme gruvbox-theme haml-mode hc-zenburn-theme
                 hcl-mode helm helm-ag helm-c-yasnippet helm-company helm-core
                 helm-css-scss helm-descbinds helm-flx helm-gitignore helm-gtags
                 helm-make helm-mode-manager helm-projectile helm-pydoc
                 helm-themes hemisu-theme heroku-theme highlight
                 highlight-indentation highlight-numbers highlight-parentheses
                 hl-todo htmlize hungry-delete hy-mode hydra hyperbole iedit
                 indent-guide inf-ruby ini-mode inkpot-theme ir-black-theme
                 jazz-theme jbeans-theme jinja2-mode js-doc js2-mode js2-refactor
                 json-mode json-reformat json-snatcher lab light-soap-theme
                 link-hint linum-relative live-py-mode livid-mode log4e
                 lorem-ipsum lush-theme lv macrostep madhat2r-theme magit
                 magit-gitflow magit-popup majapahit-theme markdown-mode
                 markdown-toc material-theme minimal-theme minitest mmm-mode
                 moe-theme molokai-theme monochrome-theme monokai-theme move-text
                 multi-term multiple-cursors mustang-theme naquadah-theme
                 noctilux-theme obsidian-theme occidental-theme oldlace-theme
                 omtose-phellack-theme open-junk-file org-bullets
                 org-category-capture org-download org-mime org-plus-contrib
                 org-pomodoro org-present org-projectile organic-green-theme orgit
                 packed paradox parent-mode pcre2el persp-mode
                 phoenix-dark-mono-theme phoenix-dark-pink-theme
                 php-auto-yasnippets php-extras php-mode phpcbf phpunit
                 pip-requirements pkg-info planet-theme popup popwin pos-tip
                 powerline professional-theme projectile prometheus-mode pug-mode
                 puppet-mode purple-haze-theme py-isort pyenv-mode pytest pythonic
                 pyvenv railscasts-theme rainbow-delimiters rake ranger rbenv
                 rebecca-theme request restart-emacs reverse-theme robe rspec-mode
                 rubocop ruby-test-mode ruby-tools rvm s sass-mode scss-mode
                 seti-theme shell-pop sicp simple-httpd skewer-mode slim-mode
                 smartparens smeargle smyx-theme soft-charcoal-theme
                 soft-morning-theme soft-stone-theme solarized-theme soothe-theme
                 spacegray-theme spaceline spinner strace-mode subatomic-theme
                 subatomic256-theme sublime-themes sunny-day-theme svg-lib tablist
                 tagedit tango-2-theme tango-plus-theme tangotango-theme tao-theme
                 terraform-mode toc-org toxi-theme transient
                 twilight-anti-bright-theme twilight-bright-theme twilight-theme
                 ujelly-theme underwater-theme use-package uuidgen vi-tilde-fringe
                 vimrc-mode volatile-highlights vterm web-beautify
                 web-completion-data web-mode which-key white-sand-theme winum
                 with-editor ws-butler xterm-color yaml-mode yapfify yasnippet
                 yasnippet-classic-snippets zen-and-art-theme zenburn-theme zones))
   '(paradox-github-token t)
   '(ranger-hidden-regexp 'nil)
   '(safe-local-variable-directories
     '("/home/brc/.emacs.d/elpa/30.1/develop/magit-20250621.2237/"
       "/data/git/flexa/infra/apps/gitlab/"))
   '(safe-local-variable-values
     '((encoding . utf-8) (eval ansible 1)
       (eval add-to-list 'company-backends 'company-ansible)))
   '(savehist-additional-variables
     '(dired-quick-sort-time-last dired-quick-sort-group-directories-last
                                  dired-quick-sort-reverse-last
                                  dired-quick-sort-sort-by-last evil-jumps-history
                                  projectile-project-command-history mark-ring
                                  global-mark-ring search-ring regexp-search-ring
                                  extended-command-history))
   '(sh-imenu-generic-expression
     '((sh
        (nil
         "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
        (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1))
       (mksh
        (nil
         "^\\s-*function\\s-+\\([^]\0\11\12 \"-$&-*/;-?[\\`|]+\\)\\s-*\\(?:()\\)?"
         1)
        (nil "^\\s-*\\([^]\0\11\12 \"-$&-*/;-?[\\`|]+\\)\\s-*()" 1))))
   '(sh-indent-after-continuation 'always)
   '(sh-shell-file "/opt/homebrew/bin/bash")
   '(treemacs-git-mode t)
   '(vterm-max-scrollback 100000)
   '(warning-suppress-types '((emacs) (comp)))
   '(xterm-color-use-bold-for-bright t))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:background nil))))
   '(evil-search-highlight-persist-highlight-face ((t (:background "magenta" :foreground "brightyellow" :underline t))))
   '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t)
   '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#444444" :foreground "#87D700"))))
   '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#FFDEC8" :foreground "red"))))
   '(ztreep-diff-model-add-face ((t (:foreground "deep sky blue")))))
  )
