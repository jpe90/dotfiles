export _JAVA_AWT_WM_NONREPARENTING=1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; fi
