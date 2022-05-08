#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# make a smarter shell
shopt -s nocaseglob
shopt -s extglob
shopt -s checkwinsize

# Not sure why comint shell has echo disabled for bash (but not zsh)
if [ -n "$INSIDE_EMACS" ]; then
    stty echo
fi
