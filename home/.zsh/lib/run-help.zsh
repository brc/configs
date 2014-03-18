if alias run-help >/dev/null; then
    unalias run-help
fi

autoload -U run-help
autoload run-help-git
autoload run-help-sudo
autoload run-help-openssl
