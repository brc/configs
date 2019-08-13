#     ____      ____
#    / __/___  / __/
#   / /_/_  / / /_
#  / __/ / /_/ __/
# /_/   /___/_/-completion.zsh
#
# - $FZF_TMUX               (default: 0)
# - $FZF_TMUX_HEIGHT        (default: '40%')
# - $FZF_COMPLETION_TRIGGER (default: '**')
# - $FZF_COMPLETION_OPTS    (default: empty)

export FZF_TMUX=1  # open fzf in new pane

# override find(1) command for fzf to prune .tox/ dir
_fzf_compgen_path() {
    echo "$1"
    command find -L "$1" \
        -name .git -prune -o \
        -name .svn -prune -o \
        -name .tox -prune -o \
        \( -type d -o -type f -o -type l \) -a -not -path "$1" -print \
            2>/dev/null | sed 's@^\./@@'
}

_fzf_compgen_dir() {
    command find -L "$1" \
        -name .git -prune -o \
        -name .svn -prune -o \
        -name .tox -prune -o \
        -type d -a -not -path "$1" -print \
            2>/dev/null | sed 's@^\./@@'
}

###########################################################

__fzfcmd_complete() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ] &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

__fzf_generic_path_completion() {
  local base lbuf compgen fzf_opts suffix tail fzf dir leftover matches
  # (Q) flag removes a quoting level: "foo\ bar" => "foo bar"
  base=${(Q)1}
  lbuf=$2
  compgen=$3
  fzf_opts=$4
  suffix=$5
  tail=$6
  fzf="$(__fzfcmd_complete)"

  setopt localoptions nonomatch
  dir="$base"
  while [ 1 ]; do
    if [[ -z "$dir" || -d ${~dir} ]]; then
      leftover=${base/#"$dir"}
      leftover=${leftover/#\/}
      [ -z "$dir" ] && dir='.'
      [ "$dir" != "/" ] && dir="${dir/%\//}"
      dir=${~dir}
      matches=$(eval "$compgen $(printf %q "$dir")" |
        FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse
          $FZF_DEFAULT_OPTS $FZF_COMPLETION_OPTS" \
            ${=fzf} ${=fzf_opts} -q "$leftover" |
              while read item; do
                echo -n "${(q)item}$suffix "
              done)
      matches=${matches% }
      if [ -n "$matches" ]; then
        LBUFFER="$lbuf$matches$tail"
      fi
      zle redisplay
      typeset -f zle-line-init >/dev/null && zle zle-line-init
      break
    fi
    dir=$(dirname "$dir")
    dir=${dir%/}/
  done
}

_fzf_path_completion() {
  __fzf_generic_path_completion "$1" "$2" _fzf_compgen_path \
    "-m" "" " "
}

_fzf_dir_completion() {
  __fzf_generic_path_completion "$1" "$2" _fzf_compgen_dir \
    "" "/" ""
}

_fzf_feed_fifo() (
  command rm -f "$1"
  mkfifo "$1"
  cat <&0 > "$1" &
)

_fzf_complete() {
  local fifo fzf_opts lbuf fzf matches post
  fifo="${TMPDIR:-/tmp}/fzf-complete-fifo-$$"
  fzf_opts=$1
  lbuf=$2
  post="${funcstack[2]}_post"
  type $post > /dev/null 2>&1 || post=cat

  fzf="$(__fzfcmd_complete)"

  _fzf_feed_fifo "$fifo"
  matches=$(
    cat "$fifo" \
     |FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} \
       --reverse $FZF_DEFAULT_OPTS $FZF_COMPLETION_OPTS" ${=fzf} \
         ${=fzf_opts} -q "${(Q)prefix}" \
           |$post | tr '\n' ' '
  )
  if [ -n "$matches" ]; then
    LBUFFER="$lbuf$matches"
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  command rm -f "$fifo"
}

# Hostname completion.
#   Each function name will be used dynamically for a command of the same name.
#       eg.:  _fzf_complete_foo() ->  foo
#
# TODO: can default _hosts function be used somehow?  as input to fzf
_fzf_complete_hostname() {
  _fzf_complete '+m' "$@" < <(
    #command cat <(cat ~/.ssh/config /etc/ssh/ssh_config 2> /dev/null | command grep -i '^host' | command grep -v '*' | awk '{for (i = 2; i <= NF; i++) print $1 " " $i}') \
    #    <(command grep -oE '^[[a-z0-9.,:-]+' ~/.ssh/known_hosts | tr ',' '\n' | tr -d '[' | awk '{ print $1 " " $1 }') \
    #    <(command grep -v '^\s*\(#\|$\)' /etc/hosts | command grep -Fv '0.0.0.0') |
    #    awk '{if (length($2) > 0) {print $2}}' | sort -u
    #
    echo "$hosts" |tr ' ' '\n' |sort -u  # see ~/.zsh/lib/50-completion.zsh
  )
}

# TODO: metaprogram this.
# TODO: figure out username-host completion for scp/rsync

_fzf_complete_ping() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_dig() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_host() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_traceroute() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_tcpraceroute() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_tcpdump() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_nmap() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_ssh() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_gnash() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_scp() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_rsync() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_nc() {
    _fzf_complete_hostname "$@"
}
_fzf_complete_telnet() {
    _fzf_complete_hostname "$@"
}

# Variable and alias completion
_fzf_complete_export() {
  _fzf_complete '-m' "$@" < <(
    declare -xp | sed 's/=.*//' | sed 's/.* //'
  )
}

_fzf_complete_unset() {
  _fzf_complete '-m' "$@" < <(
    declare -xp | sed 's/=.*//' | sed 's/.* //'
  )
}

_fzf_complete_unalias() {
  _fzf_complete '+m' "$@" < <(
    alias | sed 's/=.*//'
  )
}

fzf-completion() {
  local tokens cmd prefix trigger tail fzf matches lbuf d_cmds
  setopt localoptions noshwordsplit noksh_arrays noposixbuiltins

  # http://zsh.sourceforge.net/FAQ/zshfaq03.html
  # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion-Flags
  tokens=(${(z)LBUFFER})
  if [ ${#tokens} -lt 1 ]; then
    zle ${fzf_default_completion:-expand-or-complete}
    return
  fi

  cmd=${tokens[1]}

  # Explicitly allow for empty trigger.
  trigger=${FZF_COMPLETION_TRIGGER-'**'}
  [ -z "$trigger" -a ${LBUFFER[-1]} = ' ' ] && tokens+=("")

  tail=${LBUFFER:$(( ${#LBUFFER} - ${#trigger} ))}
  # Completion for kill(1) (do not require trigger sequence)
  if [ $cmd = kill -a ${LBUFFER[-1]} = ' ' ]; then
    fzf="$(__fzfcmd_complete)"
    matches=$(
        command ps -ef \
            |sed 1d \
            |FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-50%} \
                --min-height 15 --reverse $FZF_DEFAULT_OPTS \
                --preview 'echo {}' \
                --preview-window down:3:wrap $FZF_COMPLETION_OPTS" \
                ${=fzf} -m \
            |awk '{print $2}' \
            |tr '\n' ' '
    )
    if [ -n "$matches" ]; then
      LBUFFER="$LBUFFER$matches"
    fi
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
  # Trigger sequence given
  elif [ ${#tokens} -gt 1 -a "$tail" = "$trigger" ]; then
    d_cmds=(${=FZF_COMPLETION_DIR_COMMANDS:-cd pushd rmdir})

    [ -z "$trigger"      ] && prefix=${tokens[-1]} || prefix=${tokens[-1]:0:-${#trigger}}
    [ -z "${tokens[-1]}" ] && lbuf=$LBUFFER        || lbuf=${LBUFFER:0:-${#tokens[-1]}}

    if eval "type _fzf_complete_${cmd} > /dev/null"; then
      eval "prefix=\"$prefix\" _fzf_complete_${cmd} \"$lbuf\""
    elif [ ${d_cmds[(i)$cmd]} -le ${#d_cmds} ]; then
      _fzf_dir_completion "$prefix" "$lbuf"
    else
      _fzf_path_completion "$prefix" "$lbuf"
    fi
  # Fall back to default completion
  else
    zle ${fzf_default_completion:-expand-or-complete}
  fi
}

[ -z "$fzf_default_completion" ] && {
  binding=$(bindkey '^I')
  [[ $binding =~ 'undefined-key' ]] || fzf_default_completion=$binding[(s: :w)2]
  unset binding
}

zle     -N   fzf-completion
bindkey '^I' fzf-completion


__fzf_use_tmux__() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

__fzfcmd() {
  __fzf_use_tmux__ &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
  selected=( $(fc -rl 1 |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS \
        -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS \
        --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget
