function current_branch {
  git symbolic-ref --short HEAD
}

# git
alias g=git
alias gm='git --no-pager'

# log
alias gl='git log --stat'
alias glm='gm log'

alias gll='git log --oneline'
alias gllm='gm log --oneline'

alias glg='git log --grep'
alias glgm='gm log --grep'

# show
alias go='git show'
alias gom='gm show'

# whatchanged
alias gw='git whatchanged -p --pretty=medium'

# fetch
alias gf='git fetch'
alias gfu='git fetch -v upstream'

# pull
alias gp='git pull'
alias gpu='git pull upstream'

# remote
#alias gr='git remote'  # conflicts with `grep -rsiI' alias
alias grv='git remote -v'

# status
alias gg='git status'

# stash
alias gt='git stash'
alias gts='git stash show'
alias gtp='git stash pop'

# checkout
alias gco='git checkout'
alias gcom='git checkout master'
alias gcob='git checkout -b'

# diff
alias gd='git diff'
alias gdm='gm diff'
alias gdc='git diff --cached'
alias gdcm='gm diff --cached'

# add
alias ga='git add'
alias gaa='git add -A'
alias gau='git add -u'

# reset
alias grh='git reset HEAD'

# cherry
alias gcp='git cherry-pick'

# commit
alias gc='git commit'
alias gca='git commit -a'
alias gcA='git commit --amend'
alias gcaA='git commit -a --amend'

# push
alias gpp='git push'
alias gppo='git push origin $(current_branch):$(current_branch)'

# pull-request
alias hpr='hub pull-request -b'

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
