import XMonad
import Graphics.X11.ExtraTypes.XF86 
import XMonad.Util.EZConfig ( additionalKeys )

main = xmonad $ def
    { borderWidth           = 2
    , modMask               = mod4Mask
    , terminal              = "st"
    , normalBorderColor     = "#cccccc"
    , focusedBorderColor    = "#d08770"
    } `additionalKeys` [ 
          ((mod4Mask .|. shiftMask, xK_q     ), kill)
        , ((0,xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ((0,xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ((0,xF86XK_MonBrightnessUp),  spawn "light -A 5")
        , ((0,xF86XK_MonBrightnessDown),  spawn "light -U 5")
    ]
