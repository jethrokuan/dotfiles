#!/bin/bash

emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t > /dev/null
if [ "$?" = "1" ]; then
    emacsclient -c -n -e "(org-capture nil \"t\")" > /dev/null &
else
    emacsclient -n -e "(org-capture nil \"t\")" > /dev/null & 
fi
