#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults a
if [ -f $sysresources ]; then

    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xrandr --output eDP-1 --mode 1920x1080
exec blueberry-tray &
feh --bg-fill /home/solaire/pictures/1588334104580.jpg
picom &
xset r rate 200 45
# exec xmonad
# dwmstatus 2>&1 >/dev/null &
# exec dwm
exec i3
# exec guile-wm
# exec guile -s /home/solaire/development/scheme/guile-xcb/xcb/xml/sample/tinywm.scm
# export SBCL_HOME=/usr/lib/sbcl
# exec stumpwm
# exec sbcl --load /home/solaire/.stumpwm.d/startstump
