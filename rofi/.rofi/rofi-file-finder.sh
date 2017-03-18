#!/usr/bin/env bash

if [ -z $@ ]
then
    rg $HOME --files
else 
    xdg-open "$@" > /dev/null &
fi
