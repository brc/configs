# vim: set tags=~/.zsh/tags :

autoload -U colors && colors

# values to make a status line for indicating the current zle vi mode;
# we're gonna emulate us some vim showmode!
viins="%{${fg[white]}%}-- INSERT --%{${reset_color}%}"
vicmd="%{${fg[red]}%}-- COMMAND --%{${reset_color}%}"
vimode="${viins}"

# detect when zle changes keymap and set status line accordingly,
# then re-evaluate/expand all prompts
function zle-keymap-select {
    vimode="${${KEYMAP/vicmd/${vicmd}}/(main|viins)/${viins}}"
    zle reset-prompt
}

# always set status line to "insert" after reading command buffer/input
function zle-line-finish {
    vimode="${viins}"
}

# catch SIGINT and set status line back to insert (since the widgets above don't
# get called when interrupt is sent, i think); then repropagate the signal.
function TRAPINT {
    vimode="${viins}"
    return $(( 128 + ${1} ))
}

# this hook runs after command has been read, but before it's been executed
function preexec {
    # wipe out vi mode line (erase to end of line)
    print -rn -- "${terminfo[el]}"
}

# this hook runs before every prompt
# (but not necessarily every time it is redrawn)
#function precmd {
#    vcs_info
#}

# define the widgets
zle -N zle-keymap-select
zle -N zle-line-finish


# terminfo sequence for: cursor down, cursor up, save cursor, cursor down
terminfo_down_sc="${terminfo[cud1]}\
${terminfo[cuu1]}\
${terminfo[sc]}\
${terminfo[cud1]}"

# place vi mode status line below prompt:
# cursor down, <showmode>, restore cursor.
# (this line has to be in single quotes here)
mode='%{${terminfo_down_sc}${vimode}${terminfo[rc]}%}'

# variables for $PROMPT
host="%{${fg[red]}%}%m%{${reset_color}%}"
cwdpath="%{%U${fg[green]}%}%~%{${reset_color}%u%}"  # %~
close="%{${fg[blue]}%}>%{${reset_color}%} "        # >

PROMPT="${mode}${host} ${cwdpath}${close}"

GIT_PS1_SHOWDIRTYSTATE='true'
GIT_PS1_SHOWSTASHSTATE='true'
GIT_PS1_SHOWCOLORHINTS='true'
GIT_PS1_SHOWUPSTREAM='true'
GIT_PS1_SHOWUNTRACKEDFILES='true'
GIT_PS1_DESCRIBE_STYLE='branch'

source ~/.zsh/lib/50-git-prompt.zsh

RPROMPT='$(__git_ps1 "(%s)")'

#RPROMPT='${vcs_info_msg_0_}'
