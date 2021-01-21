#!/bin/bash

set -e

DIR=/etc/dnsmasq.d
CONF="${DIR}"/50-logmein-vpn.conf
NS=10.19.145.138

mkdir -p "${DIR}"
cat >"${CONF}" <<__EOF__
server=/logmein.com/expertcity.com/getgotools.net/goto.com/3amlabs.net/lmiprod.com/${NS}
server=/10.in-addr.arpa/${NS}
__EOF__

cat "${CONF}"  # show config

systemctl restart dnsmasq
