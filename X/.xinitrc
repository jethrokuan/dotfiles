setxkbmap dvorak

xrdb -merge ~/.Xresources &
emacs --daemon &
dropbox --start &
nitrogen --restore &

exec /usr/bin/ratpoison
