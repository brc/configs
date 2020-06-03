#!/bin/sh

set -x

DOCKER_DEV=docker0
DOCKER_NET=172.17.0.0/16
TUNDEV="${TUNDEV:-tun0}"

if /bin/grep -qw "^${DOCKER_DEV}" /proc/net/dev
then
    # XXX ip-route(8) will return 0 even when no route matches;
    #     need to grep for it.
    #
    if /sbin/ip route show table main exact "${DOCKER_NET}" dev "${TUNDEV}" \
       |/bin/grep -qw "^${DOCKER_NET}"
    then
        /sbin/ip route delete "${DOCKER_NET}"
        /sbin/ip route add "${DOCKER_NET}" dev "${DOCKER_DEV}"
    fi
fi
