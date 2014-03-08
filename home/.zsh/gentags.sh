#!/bin/sh

cd $(dirname $0)
ctags --exclude=.zcompdump* --exclude=cache --exclude=aliases \
    --exclude=gentags.sh -R . 
