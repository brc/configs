#exec gnome-session

#xsetroot -cursor_name left_ptr &
setxkbmap dvorak &
lxpanel &
#batti &
xset +fp /usr/share/fonts/local
xset fp rehash

# Lower key-repeat rate to run in VirtualBox
#xset r rate 180 100 &
xset r rate 180 50 &

xmodmap /home/duck/.xmodmaprc &
xrdb -merge /home/duck/.Xresources
terminator &
#xscreensaver </dev/null &>/dev/null &

# For running in VirtualBox
VBoxClient-all &

exec openbox
