#
# ~/.zprofile
#

zmodload zsh/zprof

# Don't export Ruby variables--it just makes a mess with chruby, rbenv, etc.,
# and the technique of mixing versions into a single $GEM_PATH doesn't work.
#
# Saving for future reference:
#
#    ruby_version=3.0.0
#    export GEM_HOME=~/.gem/ruby/${ruby_version}
#    system_gems=/usr/lib/ruby/gems/${ruby_version}
#    gempaths=(
#        ~/.gem/ruby/2.7.0
#        ~/.gem/ruby/3.0.0
#        ${GEM_HOME}
#        ${system_gems}
#    )
#    export GEM_PATH=$(printf ":%s" "${gempaths[@]}" |cut -b2-)

if [ "$(hostname)" = bunker ]; then
    GOPATH=/data/go
    GOCACHE=/data/go/.cache
else
    GOPATH="${HOME}/go"
fi

hb=/opt/homebrew
mypaths=(
    ~/bin
    ~/.local/bin
    /data/npm/bin
    "${hb}"/opt/ruby/bin
    "${hb}"/opt/gnu-sed/libexec/gnubin
    "${hb}"/opt/python@3.12/libexec/bin
    "${hb}"/share/google-cloud-sdk/bin
    ~/.krew/bin
    ~/.ebcli-virtual-env/executables
    $([ -d "${hb}" ] && "${hb}"/opt/ruby/bin/gem env gempath |cut -f1 -d:)/bin
    /usr/local/sbin
    /usr/local/bin
    /git/git-when-merged/bin
    /bin
    /sbin
    /usr/bin
    /usr/sbin
    /usr/bin/vendor_perl
    /usr/bin/core_perl
    /git/emcrubicon/campbb6/dev-utils/bin
    /git/invsblduck/fakecloud
    /git/powerline/scripts
    ${GOPATH}/bin
    /dr/bin  # rachio
    /gi/sh-lib/bin
)
export PATH=$(printf ":%s" "${mypaths[@]}" |cut -b2-)

if [ -d "${hb}" ]; then
    eval "$("${hb}"/bin/brew shellenv)"
fi

if [ -n "$TMUX" ]; then
    export TERM=screen-256color
fi

# for most pttys i use (xterm, xterm-color, screen, etc),
# the kbs sequence is ^H in the terminfo.
#stty erase ^H  # going down the rabbit hole of terminal emacs

#export EDITOR=vim  # see comment immediately above! and lib/99-emacs.zsh
#export MANPAGER="bash -c 'col -bx | bat -l man -p'"  # using batman(1) now
export MANWIDTH=80

#export MAKEFLAGS="-j$(nproc)"

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_COLLATE=C
export LC_CTYPE=C

export LESSCHARSET=utf-8
export LESS="-QRFim -j4"
# termcap codes for less(1)
# double-bright (headings, keywords, options, symbols)
#export LESS_TERMCAP_md=$'\E[01;37m'     # begin
#export LESS_TERMCAP_me=$'\E[0m'         # end
## underline (arguments and filenames)
#export LESS_TERMCAP_us=$'\E[04;36m'     # begin
#export LESS_TERMCAP_ue=$'\E[0m'         # end
## standout (highlighted search terms and info bar at bottom)
#export LESS_TERMCAP_so=$'\E[01;44;33m'  # begin
#export LESS_TERMCAP_se=$'\E[0m'         # end

export BAT_THEME='Solarized (light)'
export MANPAGER="bash -c 'col -bx | bat -l man -p'"

[ -e ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=FALSE
#export JENKINS_URL=http://build.monkeypuppetlabs.com:8080/
unset SSH_ASKPASS
export FIRE_USER=bcampbell  # LMI
export ANSIBLE_NOCOWS=true
export ANSIBLE_FORCE_COLOR=true
export RIPGREP_CONFIG_PATH=~/.config/ripgrep/config
export GTAGSLABEL=pygments
export KUBECTL_EXTERNAL_DIFF='colordiff -N -U3'

#export PYTHONDONTWRITEBYTECODE=1
export VIRTUAL_ENV_DISABLE_PROMPT=1

#export VAGRANT_DEFAULT_PROVIDER=libvirt
export VAGRANT_VM_MEM=1536
#export salt_install_args='-g https://github.com/saltstack/salt.git git v2014.7.5'

export QMK_HOME=/git/qmk/qmk_firmware

export USE_GKE_GCLOUD_AUTH_PLUGIN=True
# Let gsutil discover the version of Python it wants
# unset CLOUDSDK_PYTHON

# setup fpath
fpath=(~/.zsh/fpath $fpath)

# run other scripts
for f in $(echo ~/.zsh/lib/*.{sh,zsh} |sort); do
    source $f
done

# show fortune
if which fortune >/dev/null; then
    echo -e "\e[34m"
    if which cowsay >/dev/null; then
        fortune -s |cowsay -f stegosaurus
    else
        fortune -s
    fi
    echo -e "\e[0m"
fi

unset hb
unset mypaths

