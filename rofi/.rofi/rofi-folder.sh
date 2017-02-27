#!/bin/bash

if [ -z $@ ]
then
    find $HOME -type d
else 
    xdg-open "$@" > /dev/null &
fi
