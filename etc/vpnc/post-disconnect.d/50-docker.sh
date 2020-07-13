#!/bin/sh

set -x

DOCKER_DEV=docker0
DOCKER_NET=172.17.0.0/16
TUNDEV="${TUNDEV:-tun0}"

if grep -qw "^${DOCKER_DEV}" /proc/net/dev
then
    # XXX ip-route(8) will return 0 even when no route matches;
    #     need to grep for it.
    #
    if ! ip route show table main exact "${DOCKER_NET}" dev "${DOCKER_DEV}" \
       |grep -qw "^${DOCKER_NET}"
    then
        ip route add "${DOCKER_NET}" dev "${DOCKER_DEV}"
    fi
fi
