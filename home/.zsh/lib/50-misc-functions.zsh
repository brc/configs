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

# make sure we have our external deps
# check_prereqs() {
#     local deps=("${@}")
#     local unsatisfied=0

#     for prog in "${deps[@]}"; do
#         if echo ${prog} |/bin/grep -q /; then
#             if ! [ -e ${prog} ]; then
#                 echo "This script requires '${prog}' to exist."
#                 unsatisfied=1
#             fi
#         else
#             if ! which ${prog} &>/dev/null; then
#                 echo "Please make sure '${prog}' is in your \$PATH."
#                 unsatisfied=1
#             fi
#         fi
#     done

#     return ${unsatisfied}
# }

ok() { echo " [OK]"; }

# wait for host to respond to pings
wait_for_host() {
    local host=${1}

    echo -n "waiting for ${host} to come up..."
    # using grep allows for some packet loss
    while ! ping -c3 -l3 -W1 ${host} |grep -q '64 bytes'; do
        sleep 2
        echo -n .
    done
    ok
}

# wait for TCP port to open
wait_for_port() {
    local port=${1}
    local host=${2}

    echo -n "waiting for port ${port} on ${host}..."
    while ! nc -vvzw1 ${host} ${port} 2>/dev/null; do
        sleep 2
        echo -n .
    done
    ok
}

# wait for HTTP code
wait_for_http() {
    local code=${1}
    local url=${2}

    local rc=000
    echo -n "waiting for HTTP code ${code} from ${url} ..."
    while [ "${rc}" != ${code} ]; do
        rc=$(curl -Lks -m5 -o /dev/null -w '%{http_code}' ${url})
        if [ "${rc}" != ${code} ]; then
            sleep 3
            echo -n .
        fi
    done
    ok
}
