#!/bin/bash

if [ $# -ne 1 ]; then
    exit
fi

if [ "$1" == "start" ]; then
    if [ ! -d /var/run/vmblock-fuse ]; then
        mkdir /var/run/vmblock-fuse
    fi
    if ! mount |grep -wq vmware-vmblock; then
        /usr/bin/vmware-vmblock-fuse \
            -o subtype=vmware-vmblock,default_permissions,allow_other \
            /var/run/vmblock-fuse
    fi
fi

if [ "$1" == "stop" ]; then
    killall vmware-vmblock-fuse
fi
