# vi mode
bindkey -v

zle -N edit-command-line
autoload -Uz edit-command-line

# lower key timeout to 0.1 seconds so we can switch between vi modes faster
KEYTIMEOUT=1

# bash/emacs bindings
bindkey ^b      backward-char
bindkey ^f      forward-char
bindkey ^a      beginning-of-line
bindkey ^e      end-of-line
bindkey "\eb"   backward-word
bindkey "\ef"   forward-word
bindkey ^h      backward-delete-char
bindkey ^d      delete-char-or-list
bindkey "\ed"   kill-word
bindkey ^w      backward-kill-word
bindkey "\e^?"  backward-kill-word
bindkey ^k      kill-line
#bindkey ^u      kill-whole-line
bindkey ^t      transpose-chars
bindkey ^p      up-history
bindkey "\ep"   up-history  # compatibility with emacs comint
bindkey ^n      down-history
bindkey ^y      yank
bindkey "\ey"   yank-pop
bindkey "\e."   insert-last-word
# bindkey "\ep"   _history-complete-older  # XXX conflicts with comint above
bindkey ^r      history-incremental-search-backward
bindkey ^x^e    edit-command-line
bindkey ^xa     _expand_alias
#bindkey ^x\*   find command for glob expansion
# TODO bind all the awesome ^x bash stuff

# Make the delete key work (or Fn + Delete on the Mac)
bindkey '^?'    backward-delete-char
bindkey "\e[3~" delete-char
bindkey "\e3;5~" delete-char
bindkey "\e[3~" delete-char

# other misc mappings
bindkey -M vicmd \Y vi-yank-eol  # escaped to avoid my global alias `Y'
bindkey -M vicmd v edit-command-line
bindkey '^[[Z'  reverse-menu-complete
bindkey "\eh"   run-help
#bindkey -M menuselect ^m .accept-line      # don't press Return twice


# input macros
#function xbacklight_dim
#{
#    BUFFER='xbacklight - 10 -time 1 -steps 1'
#    zle accept-line
#}
#zle -N xbacklight_dim
#bindkey -M viins ^_d xbacklight_dim

#bindkey -s ^_d "^u xbacklight - 10 -time 1 -steps 1\n"
#bindkey -s ^_b "^u xbacklight + 10 -time 1 -steps 1\n"

bindkey -s "\ea" "|awk '{print $}'^B^B"
bindkey -s "\ec" "|count "
bindkey -s "\eg" "|grep -i "
bindkey -s "\el" "|less"
bindkey -s "\en" ">/dev/null "
bindkey -s "\eq" "pacman -Q"
bindkey -s "\er" "|recolorize_grep.sh "
bindkey -s "\es" "|sed 's///'^B^B^B"
bindkey -s "\ew" "|while read x; do ; done^B^B^B^B^B^B"
bindkey -s "\ex" "|xargs "
bindkey -s     " /"  # prevent accidental C-/ when attempting to type "SPC /" (TODO: why is it underscore?)
