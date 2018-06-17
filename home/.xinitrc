# change x11 cursor
#xsetroot -cursor_name left_ptr &

# add fonts
xset +fp /usr/share/fonts/local
xset fp rehash &

# start task panel
lxpanel &

# set xdefaults (urxvt)
xrdb -merge /home/duck/.Xresources

# fix keymap
# (don't configure dvorak anymore (ergodox))
#setxkbmap dvorak 
xmodmap /home/duck/.xmodmaprc &

# bump key repeat rate
xset r rate 180 100 &

#########################################
# THINGS TO CHANGE FOR RUNNING UNDER VM #
#########################################

# increase mouse acceleration (vmware)
xset m 8 3 &

# lower key-repeat rate (vbox)
#xset r rate 180 50 &

# shut off x11 screensaver
#xscreensaver </dev/null &>/dev/null &
xset -dpms &
xset s off &

# don't much with battery info
#batti &

# start client services (for shared clipboard, seamless mode, etc)
#VBoxClient-all &
vmware-user &
#~/bin/xrandr-fusion-laptop.sh &

#########################################
# open terminal and start wm            #
#########################################
#terminator &
xfce4-terminal &
exec openbox
