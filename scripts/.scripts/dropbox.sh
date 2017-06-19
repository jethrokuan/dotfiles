#!/usr/bin/env bash
stat=$(dropbox status)
word=""

if [[ $stat == *"Up to date"* ]]; then
    word="DONE"
elif [[ $stat == *"Upgrading"* ]]; then
    # Note: Dropbox is still running while upgrading (NixOS woes)
    word="DONE"
elif [[ $stat == *"Connecting"* ]]; then
    word="CONN"
elif [[ $stat == *"Uploading"* ]]; then
    word="UL"
elif [[ $stat == *"Downloading"* ]]; then
    word="DL"
else
    word="ERR"
fi

echo -e "$word";
