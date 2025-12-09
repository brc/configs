# -*- mode: shell-script; -*-
# vim: set ft=sh:

if inside-emacs; then
    # TODO make $LESS more emacs-friendly in case it is accidentally invoked by something else (e.g., bat(1))
    alias less=emacs-pager
    export PAGER=emacs-pager
    export SYSTEMD_PAGER=cat  # systemd won't honor /home/brc/bin/emacs-pager for some reason
fi

# Don't need this shim anymore because have emacs-pager-mode now.
#
# function bat {
#     # must use abs path to bat(1) so we don't recurse!
#     local bat_cmd="$(whereis -b bat |awk '{print $2}')"
#
#     if [ -z "${bat_cmd}" ]; then
#         >&2 echo "$0: command not found: bat"
#         return 127
#     fi
#     if inside-emacs; then
#         # don't use pager
#         "${bat_cmd}" -pp "$@"
#     else
#         "${bat_cmd}" "$@"
#     fi
# }

if ! inside-emacs; then
    have bat && alias cat=bat
fi

# aliases
#
#have exa && alias ls=exa  # incompatible ls(1) flags

if inside-emacs; then
    # enable colors for dumb terminal in comint
    export COLORTERM=true
    alias a='TERM=xterm-color ls -ah --color=auto'
    alias a.='TERM=xterm-color ls -ah --color=auto .*'
    alias ad='TERM=xterm-color ls -ah --color=auto -d'
    alias ad.='TERM=xterm-color ls -ah --color=auto -d .*'
    alias at='TERM=xterm-color ls -ah --color=auto -tr'
    alias as='TERM=xterm-color ls -ah --color=auto -Sr'

    alias aa='TERM=xterm-color ls -ah --color=auto -l'
    alias aa.='TERM=xterm-color ls -ah --color=auto -l .*'
    alias aad='TERM=xterm-color ls -ah --color=auto -l -d'
    alias aad.='TERM=xterm-color ls -ah --color=auto -l -d .*'
    alias aat='TERM=xterm-color ls -ah --color=auto -l -tr'
    alias aas='TERM=xterm-color ls -ah --color=auto -l -Sr'
else
    alias a='ls -ah --color=auto'
    alias a.='ls -ah --color=auto .*'
    alias ad='ls -ah --color=auto -d'
    alias ad.='ls -ah --color=auto -d .*'
    alias at='ls -ah --color=auto -tr'
    alias as='ls -ah --color=auto -Sr'

    alias aa='ls -ah --color=auto -l'
    alias aa.='ls -ah --color=auto -l .*'
    alias aad='ls -ah --color=auto -l -d'
    alias aad.='ls -ah --color=auto -l -d .*'
    alias aat='ls -ah --color=auto -l -tr'
    alias aas='ls -ah --color=auto -l -Sr'
fi

if have 'ls++' && ! inside-emacs
then
    # doesn't render output correctly in a comint shell, no matter the $TERM
    alias aa='ls++ --potsf -a'
    alias aa.='ls++ --potsf -a .*'
    alias aad='ls++ --potsf -a -d'
    alias aad.='ls++ --potsf -a -d .*'
    alias aat='ls++ --potsf -a -tr'
    alias aas='ls++ --potsf -a -Sr'
    alias aaat='ls++ --potsf -a -tr -L'  # deref symlinks
    alias aaas='ls++ --potsf -a -Sr -L'  #
fi

alias wa='sudo ls -ah --color=auto'
alias wa.='sudo ls -ah --color=auto .*'
alias wad='sudo ls -ah --color=auto -d'
alias wad.='sudo ls -ah --color=auto -d .*'
alias wat='sudo ls -ah --color=auto -tr'
alias was='sudo ls -ah --color=auto -Sr'

alias waa='sudo ls -ah --color=auto -l'
alias waa.='sudo ls -ah --color=auto -l .*'
alias waad='sudo ls -ah --color=auto -l -d'
alias waad.='sudo ls -ah --color=auto -l -d .*'
alias waat='sudo ls -ah --color=auto -l -tr'
alias waas='sudo ls -ah --color=auto -l -Sr'
alias waat='sudo ls -ah --color=auto -l -tr -L'  # deref symlinks
alias waas='sudo ls -ah --color=auto -l -Sr -L'  #

alias rm='rm -i'  # woopsy
alias mv='mv -i'  #
alias cp='cp -i'  #

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias pd=pushd
alias d='dirs -v'
alias 1=pushd
alias 2='pushd +2'
alias 3='pushd +3'
alias 4='pushd +4'
alias 5='pushd +5'
alias 6='pushd +6'
alias 7='pushd +7'
alias 8='pushd +8'
alias 9='pushd +9'

have colordiff && alias diff=colordiff
have lftp && alias ftp=lftp
alias gdb='gdb -q'

if ! inside-emacs; then
    if have vim; then
        alias vi=vim
        alias ivm=vim
        alias wv='sudo vim'
    else
        alias ivm=vi
        alias wv='sudo vi'
    fi
    alias v=vi
    alias vv='vi ~/.vimrc'
    alias vh='vi /etc/hosts'
    alias vssh='vi ~/.ssh/config'
else
    alias vi=emacsclient
    alias vim=emacsclient
fi

#alias find-vim-swaps="find -L . -type f -name '.*.sw[pon]'"
function find-vim-swaps {
    local dir=~/.cache/vim/swap
    [ -z "$(ls -1 "${dir}")" ] && return 0
    for f in $(ls -1tr "${dir}"); do
        echo "${dir}/${f}"
        echo "$f" |tr % / |sed 's/\.sw[pon]$//'
        echo
    done
}

alias p='ps axf'
have pstree && alias p='pstree -U'
alias pp='ps axfww'

if have pgrep; then
    a_flag=a
    if command pgrep -l${a_flag} init 2>&1 |grep -q 'invalid option'; then
        a_flag=  # fall back; -a not supported.
    fi
    alias pg="pgrep -l${a_flag}"
    alias pgg="pgrep -lf${a_flag}"
    unset a_flag
else
    function pg { command ps axww |grep "$1" |grep -vw grep; }
    function pgg { pg "$@"; }  # TODO: maybe make these funcs mimic pgrep better
fi

#alias w='sudo'  # conflicts with /dr/k8s/funcs
alias count='wc -l'
alias chx='chmod a+x'
alias chw='sudo chmod a+w'
alias chr='sudo chmod a-w'
alias chb="sudo chown ${USER}:$(groups |awk '{ print $1}')"
alias untargzip='tar xvzf'
alias untarbzip='tar xvjf'
alias untarxz='tar xvJf'
#alias n='sudo ss -ltpn'
alias n='sudo lsof -Pni 2>/dev/null |grep LIST'
alias td='sudo tcpdump -nn'

if ! inside-emacs; then
    # grep -E screws up too many sourced scripts (sh-lib) that normally work in a non-interactive shell
    alias grep='grep -E --color=auto'
fi

alias gr='grep -RIi --color --exclude-dir=.git --exclude-dir=.tox --exclude-dir=.venv'
alias wgr='sudo grep -RIi --color --exclude-dir=.git --exclude-dir=.tox --exclude-dir=.venv'

if have rg; then
    alias gr=rg
    alias grq='gr --no-filename --no-heading --no-line-number'  # STFU
    alias gri="gr --ignore -g'!.{git,tox,venv}/**'"  # use .gitignore patterns
    alias griq="grq --ignore -g'!.{git,tox,venv}/**'"
    alias wgr='sudo rg'
    alias wgri="gr --ignore -g'!.{git,tox,venv}/**'"
fi

if have tree; then
    alias t='tree -aCI .git\|.tox\|.venv'
    alias tt='tree -apDFhCI .git\|.tox\|.venv'
else
    function t {
        local p="${1:-.}"
        find "$p" -path .git -o -path .tox -o path .venv -prune -o -print
    }
fi

alias ww=~/g2sh.pl
alias r2='ssh -l root'
alias j='journalctl -xab |less +G'
alias jf='journalctl -xfab'
alias ju='journalctl -u'
alias juu='journalctl --user -u'
alias uf='systemctl list-unit-files'
alias uuf='systemctl list-unit-files --user'
alias tf='tail -f'
alias wtf='sudo tail -f'
alias tmm='tmux ls'
inside-emacs || alias tma='tmux attach'
alias be='bundle exec'

alias k=kubectl
# alias kar='k api-resources |sort'
# alias kav='k api-versions |sort'
alias kaf='k apply -f'
alias kdf='k diff -f'
# alias kpf='k port-forward'

# # kubectl get
# alias kg='k get'
# alias kgr='k get --raw'
# alias kgw='k get -w'
# alias kga='k get all'
# alias kgaa='k get all -A'
# alias kgn='kg node'
# alias kgnw='kgw node'
# alias kgns='kg namespace'
# alias kgsa='kg serviceaccount'
# alias kgcm='kg configmap'
# alias kgt='kg secret'
# alias kgds='kg daemonset'
# alias kgdsw='kgw daemonset'
# alias kgrs='kg replicaset'
# alias kgrsw='kgw replicaset'
# alias kgsts='kg statefulset'
# alias kgstsw='kgw statefulset'
# alias kgd='kg deployment'
# alias kgdw='kgw deployment'
# alias kgp='kg pods'
# alias kgpw='kgw pods'
# alias kgpa='kgp -A'
# alias kgs='kg service'
# alias kgsw='kgw service'
# alias kgi='kg ingress'
# alias kgiw='kgw ingress'
# alias kgpv='kg pv'
# alias kgpvc='kg pvc'
# alias kgpvcw='kgw pvc'
# alias kgep='kg endpoints'
# alias kgepw='kgw endpoints'
# alias kges='kg es'
# alias kgesa='kg es -A'
# alias kgesaw='kgw es -A'
# alias kgmp='kg meshpolicy'
# alias kgmw='kg mutatingwebhookconfigurations'
# alias kgoc='kg otelcol'

# # kubectl describe
# alias kd='k describe'
# alias kdn='kd node'
# alias kdns='kd namespace'
# alias kdsa='kd serviceaccount'
# alias kdcm='kd configmap'
# alias kdt='kd secret'
# alias kdds='kd daemonset'
# alias kdrs='kd replicaset'
# alias kdsts='kd statefulset'
# alias kdd='kd deployment'
# alias kdp='kd pod'
# alias kds='kd service'
# alias kdpv='kd pv'
# alias kdpvc='kd pvc'
# alias kdep='kd endpoints'
# alias kdes='kd es'
# alias kdmp='kd meshpolicy'
# alias kdmw='kd mutatingwebhookconfigurations'
# alias kdoc='kd otelcol'

# # kubectl edit
# alias ke='k edit'
# alias kesa='ke serviceaccount'
# alias kecm='ke configmap'
# alias ket='ke secret'
# alias ked='ke deployment'
# alias keds='ke daemonset'
# alias kers='ke replicaset'
# alias kests='ke statefulset'
# alias kep='ke pod'
# alias kes='ke service'
# alias keoc='ke otelcol'

# # kubectl scale
# alias ksd='kubectl scale deploy --replicas'

# # kubectl exec
# alias kit='k exec -it'

# # kubectl logs
# alias kl='k logs'

# kubectl config
# alias kcc='k config get-contexts'
# alias kuc='k config use-context'
# alias kn='k config set-context --current --namespace'

alias nat='sudo iptables -L -n -t nat'

if command dmesg -V 2>&1 |grep -q 'invalid option'; then
    # XXX which version of util-linux introduced `-H'?  same one as `-V'?!
    alias dm='dmesg |less +G'
else
    alias dm="env LESS='$LESS +G' dmesg -H"
    alias dt='dmesg -Hw'
fi

if grep -iwq ubuntu /etc/issue 2>/dev/null; then
    alias aptdate='sudo apt-get update'
    alias cache='apt-cache search'
    alias show='apt-cache show'
    alias pmi='sudo apt-get install'
elif uname -a |grep -Eq '\.el[0-9]+\.?'; then
    alias cache='yum search'
    alias show='yum info'
    alias pmi='sudo yum install'
elif [ -f /etc/arch-release ]; then
    alias aptdate='sudo pacman -Syy'
    alias cache='yay -Ss'
    alias show='yay -Si'
    alias pmi='yay -S'
elif [ "$(uname)" = Darwin ]; then
    alias aptdate='brew update'
    alias cache='brew search'
    alias show='brew info'
    alias pmi='brew install'
fi

if ! inside-emacs; then
    alias em='emacsclient -t'
fi

#
# These are _SAFE_ for emacs!
#
# !!! `vim' is alias for emacsclient inside emacs; see ~/.zsh/aliases-common
#
alias va='vim ~/.zsh/lib/10-aliases.sh'
alias na='source ~/.zsh/lib/10-aliases.sh'

alias vp='vim ~/.zprofile'
alias np='source ~/.zprofile'

alias vrc='vim ~/.zshrc'
alias nrc='source ~/.zshrc'

alias vm='vim ~/.zsh/lib/50-mappings.zsh'
# alias nm='source ~/.zsh/lib/50-mappings.zsh'  # shadows nm(1)

alias vag='vim ~/.zsh/lib/10-git-aliases.zsh'
alias nag='source ~/.zsh/lib/10-git-aliases.zsh'

alias vac='vim ~/.zsh/aliases-common'
alias nac='source ~/.zsh/aliases-common'

markdown_reader() {
    if have glow; then
        echo "glow -p"
    elif have bat; then
        echo "bat"
    else
        echo "/bin/cat"
    fi
}

get_readme() {
    local paths=(README.md README.rst README.txt README /dev/null)
    for p in "${paths[@]}"; do
        [ -e "${p}" ] && echo "${p}" && break
    done
}

rtfm() {
    if [ -n "$1" ]; then
        $(markdown_reader) "$1"
    else
        $(markdown_reader) "$(get_readme)"
    fi
}

# zsh global aliases
if is-zsh; then
    alias -g H='--help'
    alias -g CA='--color=always'
    alias -g C='|cat -A'
    alias -g Y='|bat -l yaml'
    alias -g OY='-o yaml Y'
    alias -g J='|jq . |bat -l json'
    alias -g OJ='-o json J'
    alias -g KS='-n kube-system'
    alias -g OW='-o wide |awk "{NF-=2}1" |awk "(NR==1){NF-=2; print}(NR!=1)" |column -t'
    #alias -g MW='--max-width=$(tput cols)'  # configured this as env var
fi

alias gi='cd /git/brc'
alias configs='cd /git/brc/configs/home'
alias bin='cd /git/brc/bin'

if [ "$(hostname)" = bunker ] || [ "$(hostname)" = archer ]; then
    # alias f='fixshit.sh'
    # alias wi='sudo wifi-menu'
    # alias kf='pkill plugin-containe'

    if ! inside-emacs; then
        alias tm=tmux
        alias tma='tmux attach'
        have ranger && alias r=ranger
        have bpytop && alias top=bpytop
    fi

    if [ "$(hostname)" = archer ]; then
        alias bunker='ssh bunker'
    fi

    alias mbp='ssh mbp'
    alias ww=gnash
    alias wo=/data/fire/s.sh

    alias start='sudo systemctl start'
    if ! inside-emacs; then
        # conflicts with sh-lib k8s.sh
        alias restart='sudo systemctl restart'
    fi
    alias stop='sudo systemctl stop'
    alias status='systemctl status -l'

    alias startu='systemctl --user start'
    alias restartu='systemctl --user restart'
    alias stopu='systemctl --user stop'
    alias statusu='systemctl --user status -l'

    # rax bullshit
    # alias fc='sudo fakecloud'  # conflicts with builtin history command
    alias fcc='cd /git/brc/fakecloud_configs/dotfiles'
    # alias nr='next-review -u invsblduck -l stackforge/cookbook'
    # alias cdcd='cd /git/brc/chef_dev_utils'

    # alias kc='knife client list'
    # alias kcc='knife client show'
    # alias kcd='knife client delete'

    # alias kn='knife node list'
    # alias knn='knife node show'
    # alias kne='knife node edit'
    # alias knd='knife node delete'

    # alias ke='knife environment list' # function
    # alias kee='knife environment edit'

    # rubicon bullshit
    # alias vppn='pgrep -lfa vpnc'
    # alias rubvpn='sudo vpnc drhm02'
    # alias goudavpn='sudo vpnc lsvg01'
    # alias ricevpn='sudo vpnc rice'
    # alias ge='cd /git/emcrubicon'
    # alias gec='cd /git/emcrubicon/campbb6'
    # alias geca='cd /git/emcrubicon/campbb6/plato-gouda/ansible'
    alias gecd='cd /git/emcrubicon/campbb6/dev-utils/bin'
    # alias gep='cd /git/emcrubicon/pantry-projects'
    # alias gecp='cd /git/emcrubicon/campbb6/pantry-projects'
    # alias gepd='cd /git/emcrubicon/pantry-dev'
    # alias gecpd='cd /git/emcrubicon/campbb6/pantry-dev'
    # alias gecsh='cd /git/emcrubicon/campbb6/pantry-dev/service-improvement-handbook'
    # alias ger='cd /git/emcrubicon/rubicon-saltstack'
    alias gecr='cd /git/emcrubicon/campbb6/rubicon-saltstack'
    # alias gece='cd /git/emcrubicon/campbb6/pantry-projects/ecs2-salt'
    # alias det='cd /data/emc/tmp'
    # alias detu='cd /data/emc/tmp/users-formula/test/roots'
    # alias ess='emc-saltsync.sh'
    # alias cdev='cd /data/emc/salt/dev/graft_root'
    # alias ckick='cd /data/emc/salt/dev/graft_root'
    # alias cmin='docker exec -it dev_minion_1 /bin/bash'
    # alias cmm='docker exec -it dev_master_1 /bin/bash'
    # alias ccpg='docker exec -it dev_minion_1 salt-call pillar.get'
    # alias ccpi='docker exec -it dev_minion_1 salt-call pillar.items'

    # liberty bullshit
    # alias glu='cd /git/liberty/uscm-devops'
    # alias glug='cd /git/liberty/uscm-devops/jenkins-global-groovy/vars'
    # alias glup='cd /git/liberty/uscm-devops/jenkins-python'

    # lmi bullshit
    # alias pf='cd /data/lmi/os/pf9'
    alias poc='cd /git/lmi/ie/platform9/poc/scripts'
    # alias las2crdp='xfreerdp /u:bcampbell /d:logmein /v:jump.las2c'
    # alias dn='dctl namespace set'

    # rachio bullshit
    alias dr='cd /dr'
    alias drb='cd /dr/bin'
    alias drk='cd /dr/k8s'
    alias rik='cd /gr/infrastructure-k8s'
    alias rjk='cd /gr/jenkins-k8s'
    alias rtk='cd /gr/terraform-k8s'
    alias rkc='cd /gr/kubernetes-config'

    # alias kls="ls -1 /dr/k8s/kubeconfigs/kubeconfig-*-sa \
    #     |sed -e 's#/dr/k8s/kubeconfigs/kubeconfig-##' -e 's/-sa$//' \
    #     |sort -u"

    # alias kls="(cd /dr/k8s/kubeconfigs && ls kubeconfig-*-sa \
    #     |sed -e s/kubeconfig-// -e 's/-sa$//' \
    #     |sort -u)"

    # kls() {
    #     if [[ "$1" =~ ^--?v(erbose)? ]]; then
    #         ls -1 /dr/k8s/kubeconfigs/kubeconfig-*-sa
    #     else
    #         cd /dr/k8s/kubeconfigs &&
    #             ls kubeconfig-*-sa \
    #             |sed -e s/kubeconfig-// -e 's/-sa$//' \
    #             |sort -u
    #     fi
    # }

    # alias kuc=k8s-context.sh
    # alias ktt='ktun.sh status'
    # alias kshut="ktt |grep '^status:' |awk '{print \$2}' |parallel -r ktun.sh stop"

fi #if bunker
