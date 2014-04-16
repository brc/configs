# vim: set ft=sh:

ix() { curl -F 'f:1=<-' http://ix.io < "${1:-/dev/stdin}"; }

function irclast {
    local dir='.znc/users/invsblduck/moddata/log'
    ssh chef-server "tail -${1:-20} ${dir}/\\#rcbops_$(date +%Y%m%d).log"
}

