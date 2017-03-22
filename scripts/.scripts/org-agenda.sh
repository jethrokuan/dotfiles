#!/usr/bin/env bash
emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep t > /dev/null
if [ "$?" = "1" ]; then
    emacsclient -c -n -e "(progn (org-agenda nil \" \") (delete-other-windows))" > /dev/null
else
    emacsclient -n -e "(progn (org-agenda nil \" \") (delete-other-windows))" > /dev/null
fi
