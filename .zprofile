export _JAVA_AWT_WM_NONREPARENTING=1
export EDITOR=nvim
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then exec startx; fi
