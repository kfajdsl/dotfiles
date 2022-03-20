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

if [ "$(hostname)" = "openair" ]; then
    xrandr --output DVI-D-1 --left-of HDMI-1
fi

xrandr --output DVI-D-0 --left-of HDMI-A-0
picom -b &
feh --bg-fill /home/sahan/Pictures/Bf6EPG5.jpg &
polybar bottom &
#ibus-daemon -d &
nm-applet &
wmname LG3D
flameshot &
#exec dbus-launch --exit-with-session bsp
exec bspwm
