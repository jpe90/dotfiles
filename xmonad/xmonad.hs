import Data.Map.Lazy (fromList)
import Graphics.X11.ExtraTypes.XF86
import XMonad

main =
  xmonad $
    def
      { borderWidth = 2,
        modMask = mod4Mask,
        terminal = "kitty",
        --terminal = "st",
        normalBorderColor = "#cccccc",
        focusedBorderColor = "#F44747",
        keys = \c ->
          fromList
            [ ((mod4Mask .|. shiftMask, xK_q), kill),
              ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
              ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
              ((0, xF86XK_MonBrightnessUp), spawn "light -A 5"),
              ((0, xF86XK_MonBrightnessDown), spawn "light -U 5")
            ]
            `mappend` keys defaultConfig c
      }
