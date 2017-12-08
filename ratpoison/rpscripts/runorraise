#!/bin/bash
# emulate stumpwms run-or-raise with ratpoison
# Param1: Windowname to look for
# Param2: Command to start if not present yet
# source: http://lists.gnu.org/archive/html/ratpoison-devel/2010-08/msg00005.html
# source2: https://github.com/ardumont/sh/blob/master/ratpoison/run-or-raise.sh
ratpoison -c windows|grep -q $1

if [ $? -eq 0 ] ; then
    # echo "Value grep: $? . selecting"
    ratpoison -c "select $1"
else
    # echo "Value grep: $? . starting"
    if [ -z "$2" ]; then
        # fallback, we use the same command as the window
        $1&
    else
        $2&
    fi
fi
