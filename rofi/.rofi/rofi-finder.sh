#!/usr/bin/env bash

if [ -z $@ ]
then
    fd . $HOME
else
    xdg-open "$@" > /dev/null &
fi
