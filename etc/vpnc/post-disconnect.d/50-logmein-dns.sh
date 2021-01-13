#!/bin/bash

set -e
CONF=/etc/dnsmasq.d/50-logmein-vpn.conf

rm -fv "${CONF}"

systemctl restart dnsmasq
