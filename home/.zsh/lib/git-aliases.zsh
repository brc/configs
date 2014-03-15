function current_branch {
  git symbolic-ref --short HEAD
}

# git
alias g=git
alias gm='g --no-pager'

# log
alias gl='g log --stat'
alias glm='gm log'

alias gll='g log --oneline'
alias gllm='gm log --oneline'

alias glg='g log --grep'
alias glgm='gm log --grep'

# show
alias go='g show'
alias gom='gm show'

# whatchanged
alias gw='g whatchanged -p --pretty=medium'

# fetch
alias gf='g fetch'
alias gfu='g fetch upstream'

# pull
alias gp='g pull'
alias gpu='g pull upstream'

# remote
alias gr='g remote'
alias grv='g remote -v'

# status
alias gg='g status'

# stash
alias gt='g stash'
alias gts='g stash show'
alias gtp='g stash pop'

# checkout
alias gco='g checkout'
alias gcom='g checkout master'
alias gcob='g checkout -b'

# diff
alias gd='g diff'
alias gdm='gm diff'
alias gdc='git diff --cached'
alias gdcm='gm diff --cached'

# add
alias ga='g add'
alias gaa='g add -A'

# cherry
alias gcp='g cherry-pick'

# commit
alias gc='g commit'
alias gca='g commit -a'
alias gcA='g commit --amend'
alias gcaA='g commit -a --amend'

# push
alias gpp='g push'
alias gppo='g push origin $(current_branch):$(current_branch)'

# pull-request
alias hpr='hub pull-request'

# go to root of repo
alias g/='cd $(git rev-parse --show-toplevel || echo ".")'

# Work In Progress (wip)
# These features allow to pause a branch development and switch to another one (wip)
# When you want to go back to work, just unwip it
#
# This function return a warning if the current branch is a wip
#function work_in_progress() {
#  if $(git log -n 1 2>/dev/null | grep -q -c "\-\-wip\-\-"); then
#    echo "WIP!!"
#  fi
#}
## these alias commit and uncomit wip branches
#alias gwip='git add -A; git ls-files --deleted -z | xargs -0 git rm; git commit -m "--wip--"'
#alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
