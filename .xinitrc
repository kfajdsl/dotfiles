#!/usr/bin/env sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

# merge in defaults and keymaps

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

picom -b &
feh --bg-scale /home/sahan/Pictures/Bf6EPG5.jpg &
polybar bottom &
#ibus-daemon -d &
nm-applet &
wmname LG3D
#exec dbus-launch --exit-with-session bsp
exec bspwm
