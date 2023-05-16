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
   '(nginx
     csv
     ;; ----------------------------------------------------------------
     ;; Add some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ansible
     asciidoc
     auto-completion
     docker
     emacs-lisp
     (git :variables
          git-enable-magit-delta-plugin t)
     go
     ;gtags
     helm
     html
     javascript
     kubernetes
     markdown
     org
     pdf
     perl5
     php
     protobuf
     puppet
     python
     ranger
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
     bash-completion
     company-quickhelp
     company-terraform
     ;; diff-hl
     git-gutter
     groovy-mode
     hcl-mode
     helm-icons
     helm-make  ;; I think this may already be included in Spacemacs...
     ini-mode
     magit-delta
     ox-jira
     strace-mode
     terraform-mode
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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

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
   ;; This has no effect in terminal or if "all-the-icons" package or the font
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
   ;; with 2 themes variants, one dark and one light)
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

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("SourceCode Pro"
                               :size 19
                               :weight normal
                               :width normal)

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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

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
   dotspacemacs-auto-resume-layouts t

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
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

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
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

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
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")

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

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
)


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load custom files
  ;;
  ;;  See https://www.emacswiki.org/emacs/LoadingLispFiles and
  ;;      http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el
  ;;
  (add-to-list 'load-path "~/.elisp/enabled")

  ;; Jenkinsfile mode
  (require 'jenkinsfile-mode)
  ;; (add-to-list 'auto-mode-alist '("Jenkinsfile" . jenkinsfile-mode))
  (add-to-list 'auto-mode-alist '("\\.jenkinsfile$" . jenkinsfile-mode))

  ;; Mustache mode
  (require 'mustache-mode)

  ;; Arch PKGBUILDs
  (add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

  ;; Jinja2
  (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

  ;; $PAGER support in shell-mode (see https://github.com/mbriggs/emacs-pager)
  (require 'emacs-pager)
  (add-to-list 'auto-mode-alist '("\\.emacs-pager$" . emacs-pager-mode))
  (evil-define-key nil emacs-pager-mode-map (kbd "q") 'emacs-pager-kill-pager)

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
                  '(lambda () (interactive)(mouse-yank-primary (point))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Don't transpose words with M-t
  ;; (too much collateral damage from pressing tmux macro keys in Emacs! :))
  ;;
  (global-unset-key (kbd "M-t"))
  ;;
  ;; Evil-Genius idea: Use it move around like tmux instead >:)
  ;;
  ;; TODO figure out how to loop over the keymaps and DRY it
  ;(dolist (kmap '(evil-insert-state-map
  ;                  evil-normal-state-map
  ;                  evil-visual-state-map
  ;                  evil-treemacs-state-map))
  ;  (evil-define-key nil kmap
  ;    (kbd "M-t h") 'evil-window-left
  ;    (kbd "M-t l") 'evil-window-right
  ;    (kbd "M-t j") 'evil-window-down
  ;    (kbd "M-t k") 'evil-window-up))
  (evil-define-key nil evil-insert-state-map
    (kbd "M-t h") 'evil-window-left
    (kbd "M-t l") 'evil-window-right
    (kbd "M-t j") 'evil-window-down
    (kbd "M-t k") 'evil-window-up)
  (evil-define-key nil evil-normal-state-map
    (kbd "M-t h") 'evil-window-left
    (kbd "M-t l") 'evil-window-right
    (kbd "M-t j") 'evil-window-down
    (kbd "M-t k") 'evil-window-up)
  (evil-define-key nil evil-treemacs-state-map
    (kbd "M-t h") 'evil-window-left
    (kbd "M-t l") 'evil-window-right
    (kbd "M-t j") 'evil-window-down
    (kbd "M-t k") 'evil-window-up)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Evil-mode overrides
  ;;
  ;; retain some Emacs movement bindings
  (evil-define-key nil evil-insert-state-map
    (kbd "C-a") nil  ;; unbind
    (kbd "C-b") nil  ;;
    (kbd "C-d") nil  ;;
    (kbd "C-e") nil  ;;
    (kbd "C-f") nil) ;;

  ;; don't record macros with "q" in Evil normal mode (too error prone)
  (evil-define-key nil evil-normal-state-map (kbd "q") nil)

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spacemacs overrides
  ;;
  (spacemacs/set-leader-keys
    "gg" 'magit-status
    "#"  'spacemacs/alternate-buffer
    "w#" 'spacemacs/alternate-window
    "w|" 'spacemacs/maximize-horizontally
    "or" 'brc/sh-send-line-or-region
    "oR" 'brc/sh-send-line-or-region-and-go
    "os" 'sh-show-shell
    "w_" 'brc/maximize-vertically
    "rS" 'brc/resume-last-swoop-buffer)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit overrides
  ;;
  (with-eval-after-load 'transient (transient-bind-q-to-quit))
  (evil-define-key nil magit-status-mode-map (kbd "C-n") 'magit-section-forward)
  (add-hook 'magit-process-mode-hook 'goto-address-mode)  ;; Clickable URLs
  ;; Disable line numbers for magit-delta
  ;;   see https://github.com/dandavison/magit-delta/issues/13
  ;; (add-to-list 'magit-delta-delta-args "--features=magit" t) ;; XXX value doesn't get set

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-mode overrides
  ;;
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "/" 'helm-org-rifle)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell-SCRIPT-mode (i.e., editing scripts)
  ;;
  (evil-define-key nil sh-mode-map
    (kbd "C-c C-e") 'brc/sh-send-line-or-region
    (kbd "C-c C-n") 'brc/sh-send-line-or-region-and-step)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell-mode overrides (comint process)
  ;;
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-mode-hook 'goto-address-mode)  ;; Clickable URLs
  (evil-define-key 'normal shell-mode-map (kbd "<return>") 'comint-send-input)
  (evil-define-key 'insert shell-mode-map
    (kbd "C-j") 'comint-send-input
    (kbd "C-k") 'kill-line
    (kbd "C-u") 'comint-kill-input
    (kbd "M-3") "-n r361 "
    (kbd "M-.") 'comint-insert-previous-argument
    (kbd "M-a") '(lambda ()
                   (interactive)
                   (insert "|awk '{print $}'")
                   (backward-char 2))
    (kbd "M-c") "|count"
    (kbd "M-g") "|grep -i "
    (kbd "M-h M-h") "--help"
    (kbd "M-i") "-n helm-istio-system "
    (kbd "M-k") "-n kube-system "
    (kbd "M-l") "|${PAGER}"
    (kbd "M-o") "-n otel-collector "
    (kbd "M-q") "pacman -Q"
    (kbd "M-s") "pacman -Q"
    ;(kbd "M-t") "-n tst-01 "  ;; conflicts with new window movement bindings
    (kbd "M-w") '(lambda ()
                   (interactive)
                   (insert "|while read x; do ; done")
                   (evil-backward-word-begin 2))
    (kbd "M-x") "|xargs ")


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
  ;; Misc
  ;;
  (xterm-mouse-mode -1)  ;; disable mouse for terminal
  (define-key help-map (kbd "h") nil)  ;; unbind accidental hello-file (<Esc> hh)
  (add-hook 'find-file-hook 'brc/truncate-syslog-lines)  ;; don't wrap syslog
  (helm-icons-enable)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Custom variables
  ;;
  (setq-default
   ;;create-lockfiles nil  ;; these broken symlinks wreak way too much havoc
   ;; (^ Actually: the broken symlinks are a nice indicator there is unsaved work :))
   evil-escape-key-sequence nil  ;; unbind default "fd" qwerty hack
   fci-rule-column 80  ;; vim `colorcolumn'
   scroll-margin 5     ;; vim `scrolloff'
   ;; (emacs 26.1 changed `term-char-mode' to disallow cursor movement in evil's
   ;;  normal mode--restore the previous behavior)
   term-char-mode-point-at-process-mark nil
   ;; this finds Makefile targets more correctly (using `make -nqp') than the
   ;; less-comprehensive regex in helm-make.el.
   helm-make-list-target-method 'qp
   ;; show more than 100 candidates
   helm-candidate-number-limit 250
   ;; Magit stuff
   magit-blame-echo-style 'margin
   ;; Org-mode
   org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
   ;; org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold)))
   org-todo-keyword-faces '(("INPROGRESS" . "yellow"))
   ;; Bug-fix see https://www.reddit.com/r/emacs/comments/aruxah/python_shell_doesnt_work_with_multiple_lines_of/egtwe3l/
   python-shell-prompt-block-regexp "\\.\\.\\.:? ")
   ;; magit-delta-delta-args '("--max-line-distance" "0.6" "--true-color=always" "--color-only" "--features=magit")  ;; XXX value doesn't get set
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
   '(company-quickhelp company-terraform terraform-mode hcl-mode ranger sicp yasnippet-classic-snippets zones go-guru go-eldoc flycheck-pos-tip pos-tip flycheck company-go go-mode phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode web-beautify livid-mode skewer-mode simple-httpd js2-refactor multiple-cursors js2-mode js-doc coffee-mode flyspell-correct-helm flyspell-correct auto-dictionary arch-packer dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat groovy-mode helm-gtags ggtags strace-mode ini-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data puppet-mode yaml-mode mmm-mode markdown-toc markdown-mode jinja2-mode gh-md company-ansible ansible-doc ansible xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help vimrc-mode dactyl-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme lv rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle orgit magit-gitflow magit-popup helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit transient git-commit with-editor helm-company company-statistics helm-c-yasnippet fuzzy company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
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
(defun brc/sh-send-command (go)
  "Send the current line to the inferior shell.
When region is active, send region instead.
When GO is non-nil, move point to shell process after sending text."
  (let (from to (src-window (get-buffer-window)))
    (if (use-region-p)
        (setq from (region-beginning)
              to (region-end))
      (setq from (line-beginning-position)
            to (line-end-position)))
    (sh-send-text (buffer-substring-no-properties from to))
    (if go
        (progn
            (sh-show-shell)
            (evil-insert 0))
      (select-window src-window))))

(defun brc/sh-send-line-or-region ()
  "Send the current line to the inferior shell; remain in current code window.
When region is active, send region instead."
  (interactive)
  (brc/sh-send-command nil))

(defun brc/sh-send-line-or-region-and-go ()
  "Send the current line to the inferior shell and move there.
When region is active, send region instead."
  (interactive)
  (brc/sh-send-command t))

(defun brc/sh-send-line-or-region-and-step ()
  "Send the current line to the inferior shell and move point to next line;
remain in current code window. When region is active, send region instead."
  (interactive)
  (brc/sh-send-command nil)
  (if (use-region-p)
      (goto-char (region-end))
    (goto-char (1+ (line-end-position)))))


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
;;             '(lambda () (when (boundp 'oldret)
;;                          (local-set-key "\C-m" oldret)
;;                          (makunbound 'oldret)))
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
 '(comint-move-point-for-output t)
 '(comint-process-echoes t)
 '(comint-scroll-to-bottom-on-input t)
 '(emacs-pager-max-line-coloring 5000)
 '(evil-want-Y-yank-to-eol t)
 '(exec-path
   '("/home/brc/.rbenv/bin" "/home/brc/.rbenv/shims" "/usr/local/bin" "/usr/bin" "/data/go/bin" "/usr/lib/emacs/28.1/x86_64-pc-linux-gnu"))
 '(explicit-bash-args '("-i"))
 '(helm-completion-style 'helm)
 '(js-indent-level 2)
 '(kubernetes-pod-restart-warning-threshold 2)
 '(kubernetes-poll-frequency 300)
 '(kubernetes-redraw-frequency 5)
 '(magit-delta-delta-args
   '("--max-line-distance" "0.6" "--true-color" "always" "--color-only" "--features" "magit"))
 '(magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
 '(mouse-yank-at-point t)
 '(package-selected-packages
   '(bash-completion magit-delta treemacs-magit tern company-quickhelp company-terraform terraform-mode hcl-mode ranger sicp yasnippet-classic-snippets zones go-guru go-eldoc flycheck-pos-tip pos-tip flycheck company-go go-mode phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode web-beautify livid-mode skewer-mode simple-httpd js2-refactor multiple-cursors js2-mode js-doc coffee-mode flyspell-correct-helm flyspell-correct auto-dictionary arch-packer dockerfile-mode docker json-mode tablist docker-tramp json-snatcher json-reformat groovy-mode helm-gtags ggtags strace-mode ini-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data puppet-mode yaml-mode mmm-mode markdown-toc markdown-mode jinja2-mode gh-md company-ansible ansible-doc ansible xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help vimrc-mode dactyl-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode company-anaconda anaconda-mode pythonic zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme lv rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot smeargle magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit transient git-commit with-editor helm-company company-statistics helm-c-yasnippet fuzzy company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((encoding . utf-8)
     (eval ansible 1)
     (eval add-to-list 'company-backends 'company-ansible)))
 '(sh-shell-file "/bin/bash")
 '(treemacs-git-mode t)
 '(undo-tree-incompatible-major-modes '(term-mode emacs-pager-mode))
 '(warning-suppress-types '((emacs) (comp))))
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
