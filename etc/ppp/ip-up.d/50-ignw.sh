#!/bin/bash

# TODO: add conditional logic to check $1 (ppp0) for IGNW addr?!
#       (ie., only run this script for IGNW L2TP VPN)
#
#exit 0  # XXX

ROUTES=(
    10.2.1.0/24      #main management subnet
    10.255.0.0/24    #netops admin network
    10.255.160.0/24  #aci infrastructure subnet
    10.255.254.0/24  #l3 link between core and aci
    10.3.1.0/24      #netops and storage network
    10.4.1.0/24      #ACI MGMT Subnet
    10.50.0.0/22     #ACI
    10.50.4.0/22     #static network for vlan 95
    10.50.8.0/22     #POC-Infra-Services
    172.20.0.0/16    #Pod Subnet
    172.21.0.0/16    #Pod Subnet
    172.22.0.0/16    #Pod Subnet
    172.23.0.0/16    #Pod Subnet
    172.24.0.0/16    #Pod Subnet
    172.25.0.0/16    #Pod Subnet
    172.26.0.0/16    #Pod Subnet
    172.27.0.0/16    #Pod Subnet
    172.28.0.0/16    #Pod Subnet
    172.29.0.0/16    #Pod Subnet
    10.254.250.0/24  #Management Network
    10.254.251.0/24  #Management Network
    10.254.252.0/24  #Management Network
    10.254.253.0/24  #Management Network
    10.254.254.0/24  #Management Network
    10.254.255.0/24  #Management Network
    10.254.256.0/24  #Management Network
    10.254.257.0/24  #Management Network
    10.254.258.0/24  #Management Network
    10.254.259.0/24  #Management Network
)

for net in "${ROUTES[@]}"; do
    ip route add "$net" dev "$1"
done


DIR=/etc/dnsmasq.d
CONF="${DIR}"/50-ignw-vpn.conf
NS=10.254.252.42  # TODO: How to also include .41 ?

# TODO include other in-addr.arpa zones (but not 172.16)
mkdir -p "${DIR}"
cat >"${CONF}" <<__EOF__
server=/ignw.local/${NS}
server=/10.in-addr.arpa/${NS}
__EOF__

cat "${CONF}"  # show config

systemctl restart dnsmasq
