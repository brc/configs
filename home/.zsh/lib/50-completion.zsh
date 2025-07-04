# vim: set tags=~/.zsh/tags :

WORDCHARS=''

setopt no_list_beep     # don't fucking flash before showing menu
#setopt no_menu_complete # don't autoselect the first completion entry

setopt always_to_end    # move cursor to end of word after completion
setopt auto_list        # automatically list choices on ambiguous completion
setopt auto_menu        # show completion menu on succesive tab press
setopt auto_param_keys  # dynamically remove auto-completed space when needed
setopt auto_param_slash # add slash instead of space if completing dir name
setopt no_auto_remove_slash # don't remove auto-completed trailing slash
#setopt glob_complete    # trigger menu comp for globs instead of exapanding
setopt glob_dots        # don't require leading `.' to explicitly match files
setopt list_ambiguous   # auto insert unambigous parts of completions w/o menu
setopt list_types       # show trailing character in listing to id file type

# enable completion extensions (menus, colors, etc)
zmodload -i zsh/complist

# enable bash completion support
#autoload -U bashcompinit
#bashcompinit

# normally this function comes from the `bash-completion' package;
# define a zsh-equivalent here:
_get_comp_words_by_ref ()
{
    while [ $# -gt 0 ]; do
        case "$1" in
        cur)
            cur=${COMP_WORDS[COMP_CWORD]}
            ;;
        prev)
            prev=${COMP_WORDS[COMP_CWORD-1]}
            ;;
        words)
            words=("${COMP_WORDS[@]}")
            ;;
        cword)
            cword=$COMP_CWORD
            ;;
        -n)
            # assume COMP_WORDBREAKS is already set sanely
            shift
            ;;
        esac
        shift
    done
}

# source the files in /etc/bash_completion.d
# (not sure we should source /usr/share/bash-completion/completions/*)
#setopt null_glob
#for f in /etc/bash_completion.d/*; do
#    source ${f}
#done
#unsetopt null_glob

# add useful completion styles
zstyle ':completion:*' matcher-list \
    'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*:*:*:*:processes' \
    command "ps -ax -o pid,user,comm,args |grep -v ' \[.*]\$'"

zstyle ':completion:*:*:kill:*:processes' \
    list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# enable readline equivalent of dabbrev-expand
zstyle ':completion:history-words:*:history-words' stop yes
zstyle ':completion:history-words:*:history-words' list no
zstyle ':completion:history-words:*' remove-all-dups yes
zstyle ':completion:history-words:*' menu yes

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' \
    tag-order local-directories directory-stack path-directories

cdpath=(.)

# hostname completion
# TODO: look at `_fzf_complete_hostname()' in ~/.zsh/lib/99-fzf.zsh and
#       determine which of these approaches results in a better set of
#       completions; maybe combine techniques.
_global_ssh_hosts=()
_ssh_hosts=()
_ssh_config=()
_etc_hosts=()
_dns_hosts=()

[ -r /etc/ssh/ssh_known_hosts ] && \
    _global_ssh_hosts=(${${${${(f)"$(</etc/ssh/ssh_known_hosts)"}:#[\|]*}%%\ *}%%,*})

[ -r ~/.ssh/known_hosts ] && \
    _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*})

[ -r ~/.ssh/config ] && \
    _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p'))

[ -r /etc/hosts ] && \
    : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}}

[ -r ~/.zsh/cache/dns_hosts ] && \
    _dns_hosts=($(cat ~/.zsh/cache/dns_hosts))

hosts=(
  "$_ssh_config[@]"
  "$_global_ssh_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  "$_dns_hosts[@]"
  "$HOST"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts
zstyle ':completion:*' users off

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
        dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
        hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
        mailman mailnull mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
        operator pcap postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs

# ... unless we really want to.
zstyle '*' single-ignored show

# draw ellipsis during tab completion
expand-or-complete-prefix-with-dots() {
    echo -n "\e[32m...\e[0m"
    # by default you would call the standard expand-or-complete widget here,
    # but expand-or-complete-prefix allows me to tab-complete words that have
    # trailing characters after the cursor; see
    # http://zsh.sourceforge.net/Guide/zshguide06.html#l145
    #zle expand-or-complete-prefix

    zle expand-or-complete
    zle redisplay
}
zle -N expand-or-complete-prefix-with-dots
bindkey "^I" expand-or-complete-prefix-with-dots

# allow g2sh to complete hostnames
compdef _hosts g2sh.pl gnash

# allow batman to complete man pages
compdef _man batman
