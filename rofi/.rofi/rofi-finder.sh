#!/usr/bin/env bash

if [ -z $@ ]
then
    fd --type f . $HOME
else
    xdg-open "$@" > /dev/null &
fi
