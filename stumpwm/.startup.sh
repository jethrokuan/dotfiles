if ! test -z ~/.screenlayout/desktop.sh; then
    sh ~/.screenlayout/desktop.sh 
fi

feh --bg-scale $HOME/Pictures/wallpaper.png &

compton --config $XDG_CONFIG_HOME/compton/compton.conf &

dunst &

urxvtd &

dropbox start &
