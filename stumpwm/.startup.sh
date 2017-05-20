if ! test -z ~/.screenlayout/desktop.sh; then
    sh ~/.screenlayout/desktop.sh 
fi

compton --config $XDG_CONFIG_HOME/compton/compton.conf &

dunst &

urxvtd &
