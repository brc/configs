#
# ~/.zprofile
#

export GEM_HOME=~/.gem/ruby/2.1.0
system_gems=/usr/lib/ruby/gems/2.1.0
gempaths=(
    ~/.gem/ruby/2.0.0
    ${GEM_HOME}
    ${system_gems}
)
export GEM_PATH=$(printf ":%s" "${gempaths[@]}" |cut -b2-)

mypaths=(
    ~/bin
    /usr/local/sbin
    /usr/local/bin
    /usr/bin
    /usr/bin/vendor_perl
    /usr/bin/core_perl
    $(printf "%s/bin " "${gempaths[@]}")
    /git/invsblduck/chef_dev_utils/rcb
    /git/invsblduck/chef_dev_utils/stackforge
    /git/invsblduck/chef_dev_utils/vm_kick/knife/bootstrap
    /git/invsblduck/fakecloud
    /git/powerline/scripts
)
export PATH=$(printf ":%s" "${mypaths[@]}" |cut -b2-)

if [ -n "$TMUX" ]; then
    export TERM=screen-256color
fi

stty erase ^H
export EDITOR=vim

export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_CTYPE=C

export LESSCHARSET=utf-8
export LESS="-QRfim -j4"
# termcap codes for less(1)
# double-bright (headings, keywords, options, symbols)
export LESS_TERMCAP_md=$'\E[01;37m'     # begin
export LESS_TERMCAP_me=$'\E[0m'         # end
# underline (arguments and filenames)
export LESS_TERMCAP_us=$'\E[04;36m'     # begin
export LESS_TERMCAP_ue=$'\E[0m'         # end
# standout (highlighted search terms and info bar at bottom)
export LESS_TERMCAP_so=$'\E[01;44;33m'  # begin
export LESS_TERMCAP_se=$'\E[0m'         # end

eval "$(dircolors -b ~/.dircolors)"

[ -e ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=FALSE
export JENKINS_URL=http://build.monkeypuppetlabs.com:8080/
unset SSH_ASKPASS

# setup fpath
fpath=(~/.zsh/fpath $fpath)

# load and run compinit
autoload -U compinit
compinit -i -d ~/.zsh/.zcompdump-${ZSH_VERSION}

# load functions
setopt null_glob  # don't bomb if glob expansion fails
for f in ~/.zsh/fpath/*.zsh; do
    source $f
done
setopt no_null_glob

# run other scripts
for f in ~/.zsh/lib/*.zsh; do
    source $f
done

## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M viins "\ee" edit-command-line

# git theming defaults
#ZSH_THEME_GIT_PROMPT_PREFIX="(" # prefix at very beginning of prompt
#ZSH_THEME_GIT_PROMPT_SUFFIX=")" # postfix at end of prompt
#ZSH_THEME_GIT_PROMPT_DIRTY="*"  # text to display if branch is dirty
#ZSH_THEME_GIT_PROMPT_CLEAN=""   # text to display if branch is clean

#autoload -U promptinit
#promptinit
#prompt walters

#source ~/.zsh/prompts/peepcode.zsh
#source ~/.zsh/prompts/gitprompt
source ~/.zsh/prompts/duck.zsh



# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
#HIST_STAMPS="yyyy-mm-dd"



if which fortune >/dev/null; then
    echo -e "\e[34m"
    if which cowsay >/dev/null; then
        fortune -s |cowsay -f stegosaurus
    else
        fortune -s
    fi
    echo -e "\e[0m"
fi

source ~/.zsh/aliases
