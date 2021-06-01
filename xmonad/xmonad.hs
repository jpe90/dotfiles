import XMonad
import XMonad.Util.EZConfig ( additionalKeys )

main = xmonad $ def
    { borderWidth           = 2
    , modMask               = mod4Mask
    , terminal              = "st"
    , normalBorderColor     = "#cccccc"
    , focusedBorderColor    = "#d08770"
    } `additionalKeys` [ 
          ((mod4Mask .|. shiftMask, xK_q     ), kill)
    ]
