for b in ~/.zsh/bundles/*; do
    source ${b}/*.plugin.zsh
done

ZSH_AUTOSUGGEST_STRATEGY='match_prev_cmd'
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
