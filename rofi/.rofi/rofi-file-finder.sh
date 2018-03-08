#!/usr/bin/env bash

if [ -z $@ ]
then
    rg $HOME --files -tpdf
else 
    zathura "$@" > /dev/null &
fi
