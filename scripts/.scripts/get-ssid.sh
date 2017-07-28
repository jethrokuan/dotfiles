#!/usr/bin/env sh
echo " $(nmcli -t -f name connection show --active)"
