autoload -U colors && colors

viins="%{${fg[white]}%}-- INSERT --%{${reset_color}%}"
vicmd="%{${fg[red]}%}-- COMMAND --%{${reset_color}%}"
vimode="${viins}"

function zle-keymap-select {
  vimode="${${KEYMAP/vicmd/${vicmd}}/(main|viins)/${viins}}"
  zle reset-prompt
}

function zle-line-finish {
  vimode="${viins}"
}

# catch SIGINT to set $vimode back to viins and repropagate and then signal
function TRAPINT() {
  vimode="${viins}"
  return $(( 128 + ${1} ))
}

zle -N zle-keymap-select
zle -N zle-line-finish


terminfo_down_sc="${terminfo[cud1]}\
${terminfo[cuu1]}\
${terminfo[sc]}\
${terminfo[cud1]}"

mode='%{${terminfo_down_sc}${vimode}${terminfo[rc]}%}'

open="%{${fg[black]}%}[%{${reset_color}%}"      # [
cwdpath="%{${fg[red]}%}%~%{${reset_color}%}"    # %~
close="%{${fg[black]}%}]%#%{${reset_color}%} "  # ]%#

PROMPT="${mode}${open}${cwdpath}${close}"
#RPROMPT='${vimode}'

preexec () { print -rn -- ${terminfo[el]}; }
