#
# ~/.bashrc
#

# TODO make this shared with zsh (this is copied from .zprofile)
export GOPATH=/data/go
mypaths=(
    ~/bin
    ~/.local/bin
    /data/npm/bin
    /data/gcloud/google-cloud-sdk/bin
    ~/.krew/bin
    ~/.ebcli-virtual-env/executables
    #$(printf "%s/bin " "${gempaths[@]}")
    $(gem env gempath |cut -f1 -d:)/bin
    /usr/local/sbin
    /usr/local/bin
    /git/git-when-merged/bin
    /bin
    /sbin
    /usr/bin
    /usr/sbin
    /usr/bin/vendor_perl
    /usr/bin/core_perl
    /git/emcrubicon/campbb6/dev-utils/bin
    /git/invsblduck/fakecloud
    /git/powerline/scripts
    ${GOPATH}/bin
    /dr/bin  # rachio
    /gr/sh-lib/bin  # rachio
)
export PATH=$(printf ":%s" "${mypaths[@]}" |cut -b2-)

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

eval "$(dircolors -b ~/.dircolors)"

[ -e ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=FALSE
unset SSH_ASKPASS

source /usr/share/bash-completion/bash_completion
source /data/gcloud/google-cloud-sdk/completion.bash.inc
source <(kubectl completion bash)

# Let gsutil discover the version of Python it wants
# unset CLOUDSDK_PYTHON

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
    eval $(dircolors)

    # just in case... these have historically screwed me with sh-lib
    unalias cat d restart grep 2>/dev/null

    # Not sure why comint shell has echo disabled for bash (but not zsh)
    # EDIT: Highly doubtful it's related to comint-process-echoes var, but check.
    stty echo
fi
