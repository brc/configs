# vi mode
bindkey -v

# emacs bindings
bindkey -M viins ^b     backward-char
bindkey -M viins ^f     forward-char
bindkey -M viins ^a     beginning-of-line
bindkey -M viins ^e     end-of-line
bindkey -M viins "\eb"  backward-word
bindkey -M viins "\ef"  forward-word
bindkey -M viins ^h     backward-delete-char
bindkey -M viins ^d     delete-char-or-list
bindkey -M viins "\ed"  kill-word
bindkey -M viins ^w     backward-kill-word
bindkey -M viins ^k     kill-line
bindkey -M viins ^u     kill-whole-line
bindkey -M viins ^t     transpose-chars
bindkey -M viins ^p     up-history
bindkey -M viins ^n     down-history
bindkey -M viins ^y     yank
bindkey -M viins "\ey"  yank-pop
bindkey -M viins "\e."  insert-last-word
bindkey -M viins ^r     history-incremental-search-backward
bindkey -M vicmd ^r     history-incremental-search-backward

# Make the delete key work (or Fn + Delete on the Mac)
bindkey -M viins '^?' backward-delete-char
bindkey -M viins "\e[3~" delete-char
bindkey -M viins "\e3;5~" delete-char
bindkey -M viins "\e[3~" delete-char

bindkey '^[[Z' reverse-menu-complete



# changing directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups

# completion
setopt auto_list
setopt auto_param_keys  # intelligently remove trailing space after completion
setopt auto_param_slash
setopt auto_remove_slash
setopt glob_complete    # trigger menu comp for globs instead of exapanding
setopt list_ambiguous   # auto insert unambigous parts of completions w/o menu
setopt list_types       # show trailing character identify file type
setopt no_list_beep     # don't fucking flash before showing menu

# expansion and globbing
setopt no_case_match
setopt nomatch              # bash failglob equivalent
setopt numeric_glob_sort    # sort numeric globs in "human" order
setopt rematch_pcre         # use pcre for =~

# history
setopt append_history
setopt extended_history     # `: <beginning time>:<elapsed seconds>;<command>'
setopt hist_find_no_dups    # unique events only (for ^R)
setopt hist_ignore_dups     # only for contiguous events
setopt hist_verify          # expand bang commands instead of executing them
setopt inc_append_history   # don't wait for shell exit to write history

# i/o
#setopt rc_quotes        # allow '' to signify a single quote within '...' 
setopt dvorak           # base corrections off dvorak typos instead of qwerty
setopt flow_control     # enable ^S/^Q flow control
setopt hash_cmds        # cache command locations to avoid $PATH search
setopt interactive_comments
setopt multios
setopt no_clobber
setopt rm_star_wait     # wait 10 seconds before accepting answer

# jobs
setopt check_jobs       # check for jobs before exiting shell
setopt notify           # report bg status immediately; don't wait for prompt
