#exec gnome-session

#xsetroot -cursor_name left_ptr &
setxkbmap dvorak &
lxpanel &
batti &
xset +fp /usr/share/fonts/local
xset fp rehash
xset r rate 180 100 &
xmodmap /home/duck/.xmodmaprc &
xrdb -merge /home/duck/.Xresources
terminator &
xscreensaver </dev/null &>/dev/null &

exec openbox
