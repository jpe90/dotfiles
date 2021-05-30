import XMonad

main = xmonad $ def
    { borderWidth           = 2
    , modMask               = mod4Mask 
    , terminal              = "st"
    , normalBorderColor     = "#cccccc"
    , focusedBorderColor    = "cd8b00" }
