# vim: set ft=sh:

alias vim='emacsclient -t'
alias va='vim ~/.zsh/lib/10-aliases.sh'
alias vp='vim ~/.zprofile'
alias vrc='vim ~/.zshrc'
alias vm='vim ~/.zsh/lib/mappings.zsh'
alias vag='vim ~/.zsh/lib/10-git-aliases.zsh'
alias vac='vim ~/.zsh/aliases-common'

alias na='source ~/.zsh/lib/10-aliases.sh'
alias np='source ~/.zprofile'
alias nrc='source ~/.zshrc'
alias nag='source ~/.zsh/lib/10-git-aliases.zsh'
alias nac='source ~/.zsh/aliases-common'

if [[ $(hostname) =~ archie ]]; then
    # alias f='fixshit.sh'
    # alias wi='sudo wifi-menu'
    # alias kf='pkill plugin-containe'

    # alias kc='knife client list'
    # alias kcc='knife client show'
    # alias kcd='knife client delete'

    # alias kn='knife node list'
    # alias knn='knife node show'
    # alias kne='knife node edit'
    # alias knd='knife node delete'

    # alias ke='knife environment list' # function
    # alias kee='knife environment edit'

    alias tm=tmux
    alias tma='tmux attach'

    # alias vppn='pgrep -lfa vpnc'
    # alias rubvpn='sudo vpnc drhm02'
    # alias goudavpn='sudo vpnc lsvg01'
    # alias ricevpn='sudo vpnc rice'
    # alias thinwin='rdesktop -g1024x768 -u bret6285 thinwin.rackspace.corp'

    alias ww=gnash
    alias wo=/data/fire/s.sh

    #alias fc='sudo fakecloud'  # conflicts with builtin history command
    alias jenkins='java -jar /home/duck/jenkins-cli.jar'
    alias nr='next-review -u invsblduck -l stackforge/cookbook'

    alias r=ranger

    alias gi='cd /git/invsblduck'
    alias gib='cd /git/invsblduck/bin'
    alias cdcd='cd /git/invsblduck/chef_dev_utils'
    alias fcc='cd /git/invsblduck/fakecloud_configs/dotfiles'
    alias configs='cd /git/invsblduck/configs/home'

    # zsh global aliases
    alias -g Y='|bat --color=always -l yaml'
    alias -g J='|jq -C'
    alias -g CA='--color=always'
    #alias -g MW='--max-width=$(tput cols)'  # configured this as env var


    # rubicon bullshit
    alias ge='cd /git/emcrubicon'
    alias gec='cd /git/emcrubicon/campbb6'
    alias geca='cd /git/emcrubicon/campbb6/plato-gouda/ansible'
    alias gecd='cd /git/emcrubicon/campbb6/dev-utils/bin'
    alias gep='cd /git/emcrubicon/pantry-projects'
    alias gecp='cd /git/emcrubicon/campbb6/pantry-projects'
    alias gepd='cd /git/emcrubicon/pantry-dev'
    alias gecpd='cd /git/emcrubicon/campbb6/pantry-dev'
    alias gecsh='cd /git/emcrubicon/campbb6/pantry-dev/service-improvement-handbook'
    alias ger='cd /git/emcrubicon/rubicon-saltstack'
    alias gecr='cd /git/emcrubicon/campbb6/rubicon-saltstack'
    alias gece='cd /git/emcrubicon/campbb6/pantry-projects/ecs2-salt'
    alias det='cd /data/emc/tmp'
    alias detu='cd /data/emc/tmp/users-formula/test/roots'
    alias ess='emc-saltsync.sh'
    alias cdev='cd /data/emc/salt/dev/graft_root'
    alias ckick='cd /data/emc/salt/dev/graft_root'
    alias cmin='docker exec -it dev_minion_1 /bin/bash'
    alias cmm='docker exec -it dev_master_1 /bin/bash'
    alias ccpg='docker exec -it dev_minion_1 salt-call pillar.get'
    alias ccpi='docker exec -it dev_minion_1 salt-call pillar.items'

    # liberty bullshit
    alias glu='cd /git/liberty/uscm-devops'
    alias glug='cd /git/liberty/uscm-devops/jenkins-global-groovy/vars'
    alias glup='cd /git/liberty/uscm-devops/jenkins-python'

    # lmi bullshit
    alias pf='cd /data/lmi/os/pf9'
    alias poc='cd /git/lmi/ie/platform9/poc/scripts'
    alias las2crdp='xfreerdp /u:bcampbell /d:logmein /v:jump.las2c'
fi
