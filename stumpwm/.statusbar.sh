#!/usr/bin/env bash
sleep 1;
home1='\ue802';
calendar='\ue80d';
time='\ue80f';
music='\ue81a';
headphone='\ue809';
r_arr='~/.statusbar_icons/r_arr.xbm';
while true; do
    echo "^fg(#00536B)^i(${r_arr})^fg()^bg(#00536B)\
  ^fg(#FF822E)$(printf ${home1})^fg() $(whoami)\
 ^fg(#193441)^i(${r_arr})^fg()^bg(#193441)\
  ^fg(#00A388)$(printf ${calendar})^fg() $(date +%a\ %d/%m/%y)\
 ^fg(#00A388)$(date +%H:%M)^fg()\
 ^fg(#00536B)^i(${r_arr})^fg()^bg(#00536B)\
  ^fg(#FF8A6B)$(printf ${time})^fg()\
 $(expr 183 - $(date +%j)), $(expr 136 - $(date +%j))\
 ^fg(#FF8A6B)days^fg() left\
 ^fg(#193441)^i(${r_arr})^fg()^bg(#193441)\
  ^fg(#F7E967)$(printf ${headphone})^fg() \
 ^fg(#00536B)^i(${r_arr})^fg()^bg(#00536B)\
  ^fg(#ecf0f1)$(printf ${music})^fg() $(awk -F"[][]" '/%/ { print $2 }'\
 <(amixer sget Master) | head -n1)  \
"; sleep 1; done | dzen2 -e - -h '16' -w '600' -ta r -x 1000 \
                         -fn '-*-dejavu sans with icons-normal-r-normal--*-100-80-*-p-0-utf8-1' \
                         -bg '#193441' -fg '#cbc9cf'
