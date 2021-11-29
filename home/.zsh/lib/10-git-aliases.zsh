function master_branch {
  git rev-parse master &>/dev/null && echo master || echo main
}

function current_branch {
  git symbolic-ref --short HEAD
}

# git
alias g=git

# log
alias gl='git log --stat'
alias gll='git log --oneline'
alias glg='git log --grep'
alias glum='git log --stat upstream/$(master_branch)'
alias glud='git log --stat upstream/develop'
alias glom='git log --stat origin/$(master_branch)'
alias glod='git log --stat origin/develop'

# grep
alias ggr='git grep --heading --break'

# show
alias go='git show'
alias goh='git show HEAD'
alias gohh='git show HEAD^'
alias gohhh='git show HEAD^^'

# whatchanged
alias gwc='git whatchanged -p --pretty=medium'

# fetch
alias gf='git fetch'
alias gfu='git fetch -v upstream'

# merge
alias gm='git merge'
alias gmum='git merge upstream/$(master_branch)'
alias gmud='git merge upstream/develop'
alias gmom='git merge origin/$(master_branch)'
alias gmod='git merge origin/develop'

# rebase
alias grum='git rebase upstream/$(master_branch)'
alias grud='git rebase upstream/develop'
alias grom='git rebase origin/$(master_branch)'
alias grod='git rebase origin/develop'

# pull
alias gp='git pull'
alias gpu='git pull upstream'

# branch
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gbD='git branch -D'

# remote
#alias gr='git remote'  # conflicts with `grep -rsiI' alias
alias grau='git remote add upstream'
alias grv='git remote -v'

# status
alias gg='git status'

# stash
alias gt='git stash'
alias gtl='git --no-pager stash list'
alias gts='git stash show -p'
alias gtp='git stash pop'

# checkout
alias gco='git checkout'
alias gcom='git checkout $(master_branch)'
alias gcod='git checkout develop'
alias gcob='git checkout -b'

# diff
alias gd='git diff'
alias gdc='git diff --cached'
alias gdum='git log -p upstream/$(master_branch)'
alias gdud='git log -p upstream/develop'
alias gdom='git log -p origin/$(master_branch)'
alias gdod='git log -p origin/develop'

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
alias gcA='git commit --amend --no-edit'
alias gcaA='git commit -a --amend --no-edit'
alias gcAa=gcaA

# push
alias gpp='git push'
alias gppo='git push origin $(current_branch):$(current_branch)'
alias gppf='git push origin $(current_branch):$(current_branch) --force'

# pull-request
alias hpr='hub pull-request -b'

# cd to root of repo
alias g/='cd $(git rev-parse --show-toplevel || echo ".")'

# rank authors by num commits
alias gw='git who'

# show churn areas
alias gww='git where'

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
