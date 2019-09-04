export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"           # $EDITOR opens in terminal
#export VISUAL="emacsclient -c -a emacs"  # $VISUAL opens in GUI mode

if [ -n "${EMACS}" ]; then
    export _ZSH_AUTOSUGGEST_DISABLED=true
fi
