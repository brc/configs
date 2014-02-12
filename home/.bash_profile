#
# ~/.bash_profile
#

stty erase ^H
export EDITOR=vim
export PATH=~/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/vendor_perl:/usr/bin/core_perl:~/.gem/ruby/2.0.0/bin:~/.gem/ruby/2.0.0/gems/chef-11.4.4/bin:~/.gem/ruby/2.0.0/gems/foodcritic-2.1.0/bin:/git/invsblduck/chef_dev_utils/rcb:/git/invsblduck/chef_dev_utils/vm_kick/knife/bootstrap:/git/invsblduck/fakecloud

export GEM_HOME=/home/duck/.gem/ruby/2.0.0

export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_CTYPE=C

export LESSCHARSET=utf-8
export LESS="-QRfim -j4"
# termcap codes for less(1)
# double-bright (headings, keywords, options, symbols)
export LESS_TERMCAP_md=$'\E[01;37m'     # begin
export LESS_TERMCAP_me=$'\E[0m'         # end
# underline (arguments and filenames)
export LESS_TERMCAP_us=$'\E[04;36m'     # begin
export LESS_TERMCAP_ue=$'\E[0m'         # end
# standout (highlighted search terms and info bar at bottom)
export LESS_TERMCAP_so=$'\E[01;44;33m'  # begin
export LESS_TERMCAP_se=$'\E[0m'         # end

eval "$(dircolors -b ~/.dircolors)"

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    export PROMPT_COMMAND='printf "\033]0;%s\007" "${PWD/#$HOME/~}"'
    export PS1="[\[\e[1;32m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0m\]]\$ "
    ;;
  screen*)
    export PROMPT_COMMAND='printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
    export PS1="[\[\e[1;32m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0m\]]\$ "
    ;;
esac

for f in ~/.openrc ~/.bashrc; do
    [[ -f $f ]] && . $f
done

[ -e ~/.config/ranger/rc.conf ] && export RANGER_LOAD_DEFAULT_RC=FALSE
export JENKINS_URL=http://build.monkeypuppetlabs.com:8080/
unset SSH_ASKPASS

if which fortune >/dev/null; then
    echo -e "\e[34m"
    if which cowsay >/dev/null; then
        fortune -s |cowsay -f stegosaurus
    else
        fortune -s
    fi
    echo -e "\e[0m"
fi

for f in ~/.bash/*; do
    source $f
done

