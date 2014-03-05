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
bindkey -M viins "\e[3~" delete-char    # make delete key work
bindkey -M viins "\ed"  kill-word
bindkey -M viins ^w     backward-kill-word
bindkey -M viins ^k     kill-line
bindkey -M viins ^u     kill-whole-line
bindkey -M viins ^t     transpose-chars
bindkey -M viins ^p     up-history
bindkey -M viins ^n     down-history
bindkey -M viins ^y     yank
bindkey -M viins "\ey"  yank-pop

# changing directories
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups

# completion
setopt auto_list
setopt menu_complete
setopt auto_param_keys  # intelligently remove trailing space after completion
setopt auto_param_slash
setopt auto_remove_slash
setopt glob_complete    # trigger menu comp for globs instead of exapanding
setopt list_ambiguous   # auto insert unambigous parts of completions w/o menu
setopt no_list_beep     # don't fucking flash before showing menu
setopt list_types       # show trailing character identify file type

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
setopt no_clobber
setopt dvorak           # base corrections off dvorak typos instead of qwerty
setopt flow_control     # enable ^S/^Q flow control
setopt interactive_comments
setopt hash_cmds        # cache command locations to avoid $PATH search
#setopt rc_quotes        # allow '' to signify a single quote within '...' 
setopt rm_star_wait     # wait 10 seconds before accepting answer

# jobs
setopt check_jobs       # check for jobs before exiting shell
setopt notify           # report bg status immediately; don't wait for prompt
#setopt
#setopt
#setopt
#setopt
#setopt
#setopt


# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
autoload -U promptinit
promptinit
prompt walters

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
HIST_STAMPS="yyyy-mm-dd"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(gem git github knife)
#
#source $ZSH/oh-my-zsh.sh

# User configuration

#export PATH="/home/duck/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/vendor_perl:/usr/bin/core_perl:/home/duck/.gem/ruby/2.0.0/bin:/home/duck/.gem/ruby/2.1.0/bin:/usr/lib/ruby/gems/2.1.0/bin:/git/invsblduck/chef_dev_utils/rcb:/git/invsblduck/chef_dev_utils/stackforge:/git/invsblduck/chef_dev_utils/vm_kick/knife/bootstrap:/git/invsblduck/fakecloud"
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

for f in ~/.zsh/*; do
    source $f
done
