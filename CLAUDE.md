# Goal

Migrate away from Spacemacs to a custom Emacs configuration that contains only
the Spacemacs features that I use. Create a new Emacs config from scratch that
is a subset of Spacemacs code, packages, and configurations. The new Emacs
configuration will be faster and less buggy than Spacemacs itself because we
will only be using a fraction of Spacemacs' total code. Do not minimalize or
simplify the Spacemacs code by default -- try to just copy it as-is, and later
we can analyze it for simplifications and performance improvements as a separate
project.

# Project Layout

1. The "./home/spacemacs" directory is a Git submodule of the upstream Spacemacs code repository.

1. The "./home/.spacemacs" file is my Spacemacs configuration file.

1. The "./home/.emacs.d" directory contains the new (from-scratch) configuration.

1. Please ignore anything outside of the 3 paths mentioned above for this project.

# Decisions

1. The new code base is modularized, with "core" modules and "feature" modules.

1. `package.el` is used for package management.

1. All functions borrowed Spacemacs will retain their original names so that it's easier to copy-paste code, easier to know where a function originated from, and easier to diff against upstream code.

1. Our function namespacing strategy is:
  - Use "spacemacs/" for anything copied or borrowed from Spacemacs (i.e., keep as-is);
  - Use "brc/" for custom functions; and
  - No prefix is needed for standard package functions.

  Exception: Only rename spacemacs/* functions to brc/* if you're significantly modifying the function's behavior.

1. Windows support can be removed from copied code; it is not needed.

1. An iterative approach is used to add one feature at a time to the new config.

1. After each feature is added, I will test the configuration with "emacs --init-directory=/data/git/brc/configs/home/.emacs.d -nw". Do not test emacs with -Q since 'user-emacs-directory' will be set to ~/.emacs.d and it will conflict with the existing Spacemacs install.

1. My day-to-day Emacs setup is the GUI client connected to an Emacs daemon
   (`emacsclient`). For this from-scratch config project specifically, I test in
   terminal Emacs (`-nw`) — partly because I'm often SSH'd in while working on
   it. GUI frames will be tested later. Anything *outside* this project (e.g.,
   troubleshooting my regular Spacemacs setup) should assume GUI Emacs unless I
   say otherwise.

# URGENT: Fix Transient State Implementation

## Problem Analysis
After extensive debugging, we identified that our transient state implementation is missing critical functions from Spacemacs core-transient-state.el. This causes "Unrecognized key" warnings and malformed hydra display because hydra sees `[_d_]` patterns in docstrings without proper processing.

## Root Cause
The missing `spacemacs//transient-state-remove-missing-optional-heads` function is supposed to clean up hint docstrings by removing `[_key_]` patterns that don't have corresponding bindings. Without this, hydra tries to parse every `[_d_]` as a key binding and fails.

## Implementation Plan

### Phase 1: Copy Core Transient State Functions (CRITICAL)
Copy these functions from `spacemacs/core/core-transient-state.el` to `core/transient.el`:

1. **CRITICAL**: `spacemacs//transient-state-remove-missing-optional-heads` - Removes unbound key patterns from docstrings
2. **CRITICAL**: Update `spacemacs//transient-state-make-doc` - Our version is incomplete
3. `spacemacs/transient-state-register-add-bindings` - Dynamic binding registration
4. `spacemacs/transient-state-register-remove-bindings` - Dynamic binding removal

### Phase 2: Fix Format-Hint Macro
Update `spacemacs|transient-state-format-hint` to:
- Accept `optional-heads` parameter like upstream
- Use correct `spacemacs//transient-state-make-doc` implementation

### Phase 3: Update Main Transient State Macro
Update `spacemacs|define-transient-state` to match upstream:
- Add missing properties: `:timeout`, `:idle`, `:additional-docs`
- Use `spacemacs/defer-until-after-user-config` instead of `with-eval-after-load 'hydra`
- Add `evil-declare-ignore-repeat` call
- Add `dotspacemacs-show-transient-state-title` and `dotspacemacs-show-transient-state-color-guide` support

### Phase 4: Add Missing Configuration Variables
Add to appropriate files:
- `dotspacemacs-show-transient-state-title` (default: t)
- `dotspacemacs-show-transient-state-color-guide` (default: t)
- `spacemacs/defer-until-after-user-config` function

### Phase 5: Test and Validate
- Test layouts transient state (`SPC l`)
- Verify no "Unrecognized key" warnings
- Verify proper hydra colors and formatting
- Verify all key bindings work

## Success Criteria
- No "Unrecognized key" warnings
- Transient state displays without underscores around keys
- All keys properly colored and functional
- Matches Spacemacs layouts transient state behavior exactly

# Considerations about Layouts

Missing Configuration Variables:

1. spacemacs-layouts-directory - Uses spacemacs-cache-directory instead of user-emacs-directory
2. layouts-enable-autosave and layouts-autosave-delay - Configuration for autosaving
3. spacemacs-generic-layout-names - For auto-generating layout names
4. dotspacemacs-auto-generate-layout-names - Feature for auto-generating layout names
5. spacemacs-layouts-restricted-functions - Functions that get buffer-list restriction advice
6. layouts-enable-local-variables - Feature for layout-local variables
7. Various dotspacemacs configuration variables that are referenced

Missing Functions:

1. spacemacs//activate-persp-mode - Proper activation with eyebrowse integration
2. spacemacs//layout-wait-for-modeline - Modeline integration
3. spacemacs//layout-autosave - Autosave functionality
4. spacemacs//generate-layout-name - Auto-generate layout names
5. spacemacs//layouts-ts-close-other and spacemacs//layouts-ts-kill-other - Close/kill other layouts
6. spacemacs/layout-goto-default - Jump to default layout
7. Custom layout functionality (spacemacs|define-custom-layout macro and supporting functions)
8. Project integration functions
9. Layout-local variables functionality
10. Buffer restriction advice functions
11. Functions for eyebrowse-persp integration

Missing Hooks and Advices:

I notice several critical hooks and advice mechanisms are absent from the
current implementation. These include integration points for persp-mode,
eyebrowse, and various buffer management functions. Key missing hooks involve
autosave triggers, layout switching events, and state preservation mechanisms
that ensure smooth workspace transitions and state management.

The hooks cover critical lifecycle events like before and after layout switches,
state saving and loading, and buffer renaming. They're essential for maintaining
workspace consistency and providing a seamless user experience across different
perspectives and window configurations.

Dependencies reveal significant integration gaps with winner mode, helm, ivy,
and projectile - each representing potential workflow enhancements that are
currently unimplemented. These integrations would provide more robust layout and
project management capabilities.

Transient state features are particularly underdeveloped, with multiple key
bindings missing for layout navigation, buffer management, and custom layout
interactions. The absence of a comprehensive custom layouts transient state
limits user flexibility and workflow customization.

Critical missing features include autosave mechanisms, layout-local variable
support, auto-generated layout naming, and advanced buffer restriction - all of
which are fundamental for a sophisticated workspace management system.

Project integration remains a notable gap, suggesting limited interoperability
with existing development tools and project management workflows.

● Based on my analysis of your current layouts implementation vs Spacemacs, here are the major differences and missing components:

  Missing Configuration Variables

  Critical missing variables:
  - spacemacs-layouts-directory - Should use spacemacs-cache-directory instead of user-emacs-directory
  - layouts-enable-autosave and layouts-autosave-delay - Autosave configuration
  - spacemacs-generic-layout-names - For auto-generating layout names
  - spacemacs-layouts-restricted-functions - Functions to restrict buffer-list to current layout
  - layouts-enable-local-variables - Layout-local variables feature
  - Various dotspacemacs-* configuration variables referenced throughout

  Missing Core Functions

  Major missing functions:
  - spacemacs//activate-persp-mode - Proper activation with eyebrowse integration and rename-buffer advice
  - spacemacs//layout-wait-for-modeline - Modeline integration
  - spacemacs//layout-autosave - Complete autosave functionality
  - spacemacs//generate-layout-name - Auto-generate layout names from generic names
  - spacemacs//layouts-ts-close-other / spacemacs//layouts-ts-kill-other - Close/kill other layouts
  - spacemacs/layout-goto-default - Jump to default layout
  - spacemacs/layout-switch-to - Switch to layout by number with prompting
  - All custom layout functionality (spacemacs|define-custom-layout macro, etc.)
  - Layout-local variables system (spacemacs/make-variable-layout-local, etc.)
  - Advanced buffer restriction advice functions

  Missing Hooks and Advices

  Critical missing integrations:
  - persp-mode-hook → spacemacs//layout-autosave
  - persp-before-switch-functions → eyebrowse integration
  - eyebrowse-post-window-switch-hook → perspective saving
  - persp-activated-functions → eyebrowse loading
  - persp-before-save-state-to-file-functions → eyebrowse updates
  - persp-after-load-state-functions → eyebrowse loading after restore
  - Advice on persp-load-state-from-file for modeline waiting
  - Advice on persp-switch for layout-local variables
  - Buffer-list restriction advices for multiple functions
  - rename-buffer advice for eyebrowse compatibility

  Missing Transient State Features

  Missing bindings in layouts transient state:
  - "e" → spacemacs/layout-switch-to (switch to layout by number)
  - "D" → spacemacs/layouts-ts-close-other (close other layouts)
  - "h" → spacemacs/layout-goto-default (go to default layout)
  - "o" → spacemacs/select-custom-layout (custom layouts)
  - "t" → persp-temporarily-display-buffer (temp display buffer)
  - "X" → spacemacs/layouts-ts-kill-other (kill other layouts)

  Completely missing:
  - Custom layouts transient state
  - More sophisticated hint generation

  Missing Integration Features

  Package integrations missing:
  - Winner mode layout-local variables
  - Helm/Ivy advanced buffer functions (spacemacs-layouts/non-restricted-buffer-list-*)
  - Projectile integration (spacemacs/helm-persp-switch-project, etc.)
  - Spaceline integration for layout display

  Missing Advanced Features

  Major feature gaps:
  1. Autosave system - No timer-based layout autosaving
  2. Custom layouts - No spacemacs|define-custom-layout macro or custom layout management
  3. Layout-local variables - No system for variables that are local to specific layouts
  4. Auto-generated names - No automatic layout naming from predefined name lists
  5. Advanced buffer restriction - No sophisticated buffer-list filtering for specific functions
  6. Project integration - No automatic layout creation/switching for projects

  The biggest functional gaps are the autosave system, custom layouts, and layout-local variables - these are substantial features in Spacemacs
  that are completely absent from your current implementation.

● Your organization approach makes perfect sense. Here's how I'd suggest structuring it:

  Core vs Module Organization

  core/transient.el (keep the transient state macros here since they're reusable):
  - spacemacs|define-transient-state macro
  - spacemacs|transient-state-format-hint macro
  - All supporting transient state helper functions
  - spacemacs-post-user-config-hook and related infrastructure
  - Any other macros that multiple modules might use

  modules/layouts.el (move layout-specific content here):
  - All layout configuration variables
  - All layout-specific functions
  - The actual transient state definitions (spacemacs|define-transient-state layouts and workspaces)
  - Package configurations (use-package persp-mode, use-package eyebrowse)
  - Hooks and advices specific to layouts
  - Key bindings

  This separation follows the principle that:
  - Core = reusable infrastructure (macros, common utilities)
  - Modules = feature-specific implementations using that infrastructure

  Since spacemacs|define-transient-state could be used by other features beyond layouts (like toggles, text manipulation, etc.), it belongs in core. But the actual layout transient
  states, all the layout functions, and persp-mode configuration are layout-specific and belong in the module.

  The current structure mostly follows this already - we'd just need to move the transient state definitions and some of the more layout-specific helper functions from
  core/transient.el to modules/layouts.el.

# Features from Spacemacs to be implemented

As you implement each feature below, it is expected that you will first look at how the feature is generally implemented in Spacemacs (e.g., it may be its own layer or it may be a bunch of core functions). Once you learn the entirety of how it is implemented, you and I will decide how to copy the functions to the new code base.

The current list of features to implement from Spacemacs are:

[x] package management
[x] `which-key`
[x] Global keybindings unrelated to Evil
[x] Evil mode (large tasked split into phases, described below)
[x] Status line
[x] SPC leader key (How is this implemented in Spacemacs?)
[x] Default leader keys from spacemacs core bootstrap
[x] FIX: SPC s c: Wrong type argument: commandp, spacemacs/evil-search-clear-highlight
[x] Helm
[x] SPC SPC helm-M-x-fuzzy-matching
[x] Projectile
[x] Helm packages (such as swoop, wgrep, helm-projectile-ag, etc)
[x] SPC * (search project w/input)
[ ] SPC l (layouts-transient-state)
[ ] SPC p l (helm-persp-switch-project)
[ ] Treemacs (plus my current Treemacs configuration)
[ ] Magit
[ ] LSP
[ ] Tree-sitter
[ ] Org-Mode
[ ] TODO: Implement the spacemacs functions in default-leader-keys.el?
[ ] A ton of toggles under SPC t (need to look at each one)
[ ] auto-completion
[ ] narrowing
[ ] SPC c y (spacemacs/copy-and-comment-lines)
[ ] gd (spacemacs/jump-to-definition)
[ ] gD (spacemacs/jump-to-definition-other-window)
[ ] SPC a u (vundo)
[ ] drag-stuff and J/K keybindings
[ ] K (spacemacs/evil-smart-doc-lookup)
[ ] Rifle search
[ ] SPC v (expand region)
[ ] Terraform
[ ] Themes
[ ] Spell check

# How to interact with me

Don't patronize me with phrases like, "You're absolutely right!" every time you
make a mistake and I offer a suggestion or correction. Use more of a curious
tone when you're unsure about a solution, and use a self-deprecating tone when
your 95% or more confident that my suggestion is correct. We are colleagues exploring
a problem together, matter-of-factly -- I am not delicate, and you should act
like an old sysadmin.
