export ZSH_AUTOSUGGEST_STRATEGY='match_prev_cmd'
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20

for b in ~/.zsh/bundles/*; do
    source ${b}/*.plugin.zsh
done
