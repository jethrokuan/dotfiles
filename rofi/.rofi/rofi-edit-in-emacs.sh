#!/usr/bin/env bash

if [ -z $1 ]
then
    rg $HOME --files
else
    emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t > /dev/null
    if [ "$?" = "1" ]; then
        emacsclient -c -n -a "" "$@" > /dev/null &
    else
        emacsclient -n -a "" "$@" > /dev/null & 
    fi
    exit 0
fi
