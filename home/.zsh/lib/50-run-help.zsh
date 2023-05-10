if alias run-help >/dev/null; then
    unalias run-help
fi

autoload -U run-help

# distributed with zsh
autoload run-help-ip
autoload run-help-git
autoload run-help-sudo
autoload run-help-openssl

# custom in ~/.zsh/fpath
autoload run-help-bundle
