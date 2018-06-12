#
# ~/.zprofile
#

export GOPATH=/data/go

ruby_version=2.5.0
export GEM_HOME=~/.gem/ruby/${ruby_version}
system_gems=/usr/lib/ruby/gems/${ruby_version}
gempaths=(
    ~/.gem/ruby/2.0.0
    ~/.gem/ruby/2.1.0
    ~/.gem/ruby/2.2.0
    ~/.gem/ruby/2.3.0
    ~/.gem/ruby/2.4.0
    ${GEM_HOME}
    ${system_gems}
)
export GEM_PATH=$(printf ":%s" "${gempaths[@]}" |cut -b2-)

mypaths=(
    ~/bin
    /usr/local/sbin
    /usr/local/bin
    /bin
    /sbin
    /usr/bin
    /usr/sbin
    /usr/bin/vendor_perl
    /usr/bin/core_perl
    $(printf "%s/bin " "${gempaths[@]}")
    /git/emcrubicon/campbb6/dev-utils/bin
    /git/invsblduck/fakecloud
    /git/powerline/scripts
    ${GOPATH}/bin
)
export PATH=$(printf ":%s" "${mypaths[@]}" |cut -b2-)

if [ -n "$TMUX" ]; then
    export TERM=screen-256color
fi

# for most pttys i use (xterm, xterm-color, screen, etc),
# the kbs sequence is ^H in the terminfo.
stty erase ^H

export EDITOR=vim

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
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
#export JENKINS_URL=http://build.monkeypuppetlabs.com:8080/
unset SSH_ASKPASS

export FIRE_USER=bcampbell  # LMI

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/config

#export PYTHONDONTWRITEBYTECODE=1
export VIRTUAL_ENV_DISABLE_PROMPT=1

#export VAGRANT_DEFAULT_PROVIDER=libvirt
export VAGRANT_VM_MEM=1536
#export salt_install_args='-g https://github.com/saltstack/salt.git git v2014.7.5'

# configure basic history
export HISTSIZE=10000
export SAVEHIST=9990        # smaller than HISTSIZE for hist_expire_dups_first
export HISTFILE=~/.zsh/history
setopt extended_history         # ":start:elapsed;command" format
setopt inc_append_history       # write history immediately (not after exit)
setopt hist_ignore_dups         # don't record consecutively duplicate events
setopt hist_expire_dups_first   # rotate global dups out first
setopt hist_find_no_dups        # skip dups during history search
setopt hist_verify              # expand history before executing command

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
