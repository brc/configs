# -*- mode: sh; -*-
# vim: set ft=sh:

# XXX this should probably be a relative path, since we
#     live in symlinking hell.
source ~/.zsh/aliases-common

# needed because $SHELL value gets wonky
function current-shell {
    ps -p $$ -o comm=
}

function is-bash {
    [ "$(current-shell)" = bash ]
}

function is-zsh {
    [ "$(current-shell)" = zsh ]
}

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

if [ "$(hostname)" = bunker ] || [ "$(hostname)" = archer ]; then
    # alias f='fixshit.sh'
    # alias wi='sudo wifi-menu'
    # alias kf='pkill plugin-containe'

    if ! inside-emacs; then
        alias tm=tmux
        alias tma='tmux attach'
        have ranger && alias r=ranger
        have bpytop && alias top=bpytop
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
    fi

    if [ "$(hostname)" = archer ]; then
        alias bunker='ssh bunker'
    fi

    alias mbp='ssh mbp'
    alias ww=gnash
    alias wo=/data/fire/s.sh

    alias gi='cd /git/brc'
    alias configs='cd /git/brc/configs/home'
    alias bin='cd /git/brc/bin'

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
        alias -g IS='-n helm-istio-system' # XXX rachio
        alias -g HA='-n ha-poc'            #
        alias -g DR='-n ha-poc-dr'         #
        alias -g SO='-n splunk-otel'       #
        alias -g OC='-n otel-collector'    #
        alias -g SFX='-n signalfx'         #
        alias -g MON='-n monitoring'       #
        alias -g PROM='-n prometheus'      #
        alias -g CM='-n cert-manager'      #
        alias -g TST='-n tst-01'           #
        alias -g AP='AWS_PROFILE=reltio'   #
        alias -g CP='AWS_PROFILE=cp'       #
        alias -g OP='AWS_PROFILE=op'       #
        alias -g OW='-o wide |awk "{NF-=2}1" |awk "(NR==1){NF-=2; print}(NR!=1)" |column -t'
        #alias -g MW='--max-width=$(tput cols)'  # configured this as env var
    fi

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
