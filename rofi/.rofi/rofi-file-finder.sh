#!/bin/bash

usage()
{
    cat << EOF
usage: $0 options

Index and search files using dmenu

OPTIONS:
   -h      Show this message
   -o      Store index [default: /tmp/fmenu_index]
   -i      Search folders [default: $HOME]
   -x      Exclude files [default: ".*\/..*" (i.e. dot-files)]
   -d      dmenu parameters [default: -i -l 20]
   -f      force reloading index [default: false]
   -t      time [default: 5min]
   -u      Just update the index
EOF
}

if [ -z $@ ]
then
    locate /home/jethro/
else 
    xdg-open "$@" > /dev/null &
fi
