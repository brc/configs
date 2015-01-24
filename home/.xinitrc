#exec gnome-session
#xsetroot -cursor_name left_ptr &

xrdb -merge /home/duck/.Xresources
lxpanel &
xset +fp /usr/share/fonts/local
xset fp rehash &
setxkbmap dvorak 
xmodmap /home/duck/.xmodmaprc &
terminator &

#########################################
# THINGS TO CHANGE FOR RUNNING UNDER VM #
#########################################

# Lower key-repeat rate
#xset r rate 180 100 &
xset r rate 180 50 &

# Shut off Xorg screensaver
#xscreensaver </dev/null &>/dev/null &
xset -dpms &
xset s off &

# Don't much with battery info
#batti &

# Start client services (for shared clipboard, seamless mode, etc)
#VBoxClient-all &
vmware-user

#--
exec openbox
