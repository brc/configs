# auto-escape special chars in URLs
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic
