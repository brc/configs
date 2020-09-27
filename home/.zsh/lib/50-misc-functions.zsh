# vim: set ft=sh:

ix() { curl -F 'f:1=<-' http://ix.io < "${1:-/dev/stdin}"; }

pubkey() {
    echo "# Brett's key"
    /bin/cat ~/.ssh/id_rsa.pub  # don't use bat(1)
}

#function irclast {
#    local dir='.znc/users/invsblduck/moddata/log'
#    ssh chef-server "tail -${1:-20} ${dir}/\\#rcbops_$(date +%Y%m%d).log"
#}

