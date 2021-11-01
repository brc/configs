# load completion system
autoload -Uz compinit

# name the dump file by zsh version in case it ever become incompatible.
_dumppath=~/.zsh/.zcompdump-${ZSH_VERSION}

# don't call compinit regularly every single time a shell starts--it's way too
# expensive, especially with all the security/auditing checks (see zprof
# output). only do a full run/dump if the dump file is older than 24 hrs.
# instead, run it with -C all day long (see zshcompsys(1)).
#
setopt extended_glob
if [[ -n ${_dumppath}(#qNmh-24) ]]; then
    compinit -C -d "${_dumppath}"
else
    echo dumping!
    compinit -d "${_dumppath}"
    zcompile "${_dumppath}"
fi
unsetopt extended_glob
unset _dumppath

