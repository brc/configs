function have {
    # command -v "$1" >/dev/null
    whereis "$1" |awk '{print $2}' |grep -q '^/'
}

function inside-emacs {
    [ -n "${INSIDE_EMACS}" ]
}

# needed because $SHELL value gets wonky
function current-shell {
    ps -p $$ -o comm=
}

function is-bash {
    [ "$(current-shell)" = bash ]
}

function is-zsh {
    [ "$(current-shell)" = zsh ]
}
