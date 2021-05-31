import XMonad
import qualified Data.Map        as M
import XMonad.Util.EZConfig ( additionalKeys )

main = xmonad $ def
    { borderWidth           = 2
    , modMask               = mod4Mask
    , terminal              = "st"
    , normalBorderColor     = "#cccccc"
    , focusedBorderColor    = "#fb4934"
    } `additionalKeys` [ 
          ((mod4Mask .|. shiftMask, xK_q     ), kill)
        -- we should fix this
        , ((mod4Mask, xK_Return             ), spawn $ "st")
    ]
