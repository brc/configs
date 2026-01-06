#
# ~/.bashrc
#

# TODO make this shared with zsh (this is copied from .zprofile)
if [ "$(hostname)" = bunker ]; then
    GOPATH=/data/go
    GOCACHE=/data/go/.cache
else
    GOPATH="${HOME}/go"
fi
hb=/opt/homebrew
mypaths=(
    ~/bin
    ~/.local/bin
    /data/npm/bin
    "${hb}"/opt/ruby/bin
    "${hb}"/opt/gnu-sed/libexec/gnubin
    "${hb}"/opt/python@3.12/libexec/bin
    "${hb}"/share/google-cloud-sdk/bin
    ~/.krew/bin
    #$(printf "%s/bin " "${gempaths[@]}")
    $(gem=gem
        [ -d "${hb}" ] && gem="${hb}"/opt/ruby/bin/gem
        "${gem}" env gempath |cut -f1 -d:
    )/bin
    /usr/local/sbin
    /usr/local/bin
    /git/git-when-merged/bin
    /bin
    /sbin
    /usr/bin
    /usr/sbin
    /usr/bin/vendor_perl
    /usr/bin/core_perl
    /git/powerline/scripts
    ${GOPATH}/bin
    /gi/sh-lib/bin
)
export PATH=$(printf ":%s" "${mypaths[@]}" |cut -b2-)

if [ -d "${hb}" ]; then
    eval "$("${hb}"/bin/brew shellenv)"
    export CLOUDSDK_PYTHON=/opt/homebrew/opt/python@3.12/libexec/bin/python
fi

export BAT_THEME='Solarized (light)'
export KUBECTL_EXTERNAL_DIFF='colordiff -N -U3'
export LESS="-QRFim -j4"
export LSCOLORS=Hefxcxdxbxegedabagacadah
export MANPAGER="bash -c 'col -bx | bat -l man -p'"
export USE_GKE_GCLOUD_AUTH_PLUGIN=True
export RIPGREP_CONFIG_PATH="$HOME/.config/ripgrep/config"


######################################################################
# If not running interactively, don't do anything else
######################################################################
[[ $- != *i* ]] && return

# Get our insane symlink-jungle mudball hell
for f in ~/.bash/*; do
    source $f
done

if ! inside-emacs; then
    if [ -n "$TMUX" ]; then
        export TERM=screen-256color
    fi
fi

# case ${TERM} in
#     xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
#         export PROMPT_COMMAND='printf "\033]0;%s\007" "${PWD/#$HOME/~}"'
#         ;;
#     screen*)
#         export PROMPT_COMMAND='printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
#         ;;
# esac


stty erase ^H
export EDITOR='emacsclient -a vim'

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

if have dircolors; then
    eval "$(dircolors -b ~/.dircolors)"
fi

[ -e ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=FALSE
unset SSH_ASKPASS

comp_files=(
    /usr/share/bash-completion/bash_completion
    /opt/google-cloud-cli/completion.bash.inc
)
for f in "${comp_files[@]}"; do
    [ -f "$f" ] && source "$f"
done
source <(kubectl completion bash)

if which fortune >/dev/null; then
    echo -e "\e[34m"
    if which cowsay >/dev/null; then
        fortune -s |cowsay -f stegosaurus
    else
        fortune -s
    fi
    echo -e "\e[0m"
fi

export PS1="[\[\e[1;32m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0m\]]\$(__k8s_ps1_current_context)\$ "

# make a smarter shell
shopt -s nocaseglob
shopt -s extglob
shopt -s checkwinsize

if inside-emacs; then
    export SHELL=/bin/bash
    unset PROMPT_COMMAND

    # use default dircolors database (renders better with ef-dream theme)
    # eval $(dircolors)

    # just in case... these have historically screwed me with sh-lib
    unalias cat d restart grep 2>/dev/null

    # Not sure why comint shell has echo disabled for bash (but not zsh)
    # EDIT: Highly doubtful it's related to comint-process-echoes var, but check.
    stty echo
fi

unset hb mypaths comp_files
