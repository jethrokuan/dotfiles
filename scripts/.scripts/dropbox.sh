#!/bin/bash

if dropbox status | grep -q "Up to date";
then
    word=DONE
else
    if dropbox status | grep -q Downloading;
    then
        word=DL
    elif dropbox status | grep -q Uploading;
    then
        word=UL
    else
        word=ERR
    fi
fi

echo -e "DB: $word";
