# vi mode
bindkey -v

# emacs bindings
bindkey -M viins ^b     backward-char
bindkey -M viins ^f     forward-char
bindkey -M viins ^a     beginning-of-line
bindkey -M viins ^e     end-of-line
bindkey -M viins "\eb"  backward-word
bindkey -M viins "\ef"  forward-word
bindkey -M viins ^h     backward-delete-char
bindkey -M viins ^d     delete-char-or-list
bindkey -M viins "\ed"  kill-word
bindkey -M viins ^w     backward-kill-word
bindkey -M viins ^k     kill-line
bindkey -M viins ^u     kill-whole-line
bindkey -M viins ^t     transpose-chars
bindkey -M viins ^p     up-history
bindkey -M viins ^n     down-history
bindkey -M viins ^y     yank
bindkey -M viins "\ey"  yank-pop
bindkey -M viins "\e."  insert-last-word
bindkey -M viins ^r     history-incremental-search-backward
bindkey -M vicmd ^r     history-incremental-search-backward

# Make the delete key work (or Fn + Delete on the Mac)
bindkey -M viins '^?' backward-delete-char
bindkey -M viins "\e[3~" delete-char
bindkey -M viins "\e3;5~" delete-char
bindkey -M viins "\e[3~" delete-char

bindkey '^[[Z' reverse-menu-complete

# input macros
#function xbacklight_dim
#{
#    BUFFER='xbacklight - 10 -time 1 -steps 1'
#    zle accept-line
#}
#zle -N xbacklight_dim
#bindkey -M viins ^_d xbacklight_dim

bindkey -s ^_d "^u xbacklight - 10 -time 1 -steps 1\n"
bindkey -s ^_b "^u xbacklight + 10 -time 1 -steps 1\n"

bindkey -s "\eg" "i|grep -i "
bindkey -s "\eg" "i|grep -i "

bindkey -s "\ea" "|awk '{print $}'^B^B"
bindkey -s "\eg" "|grep -i "
bindkey -s "\el" "|less"
bindkey -s "\en" ">/dev/null "
bindkey -s "\eq" "pacman -Q"
bindkey -s "\es" "|sed 's///'^B^B^B"
bindkey -s "\ew" "|while read x; do ; done^B^B^B^B^B^B"
bindkey -s "\er" "|recolorize_grep.sh "
bindkey -s "\ex" "|xargs "
bindkey -s "\ec" "|count "
