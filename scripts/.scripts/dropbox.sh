#!/bin/bash
stat=$(dropbox status)

if   echo $stat | grep -q "Up to date"; then word=DONE
elif echo $stat | grep -q "Downloading"; then word=DL
elif echo $stat | grep -q "Connecting"; then word=CONN
elif echo $stat | grep -q "Uploading"; then word=UL
else word=ERR
fi

echo -e "DB: $word";
