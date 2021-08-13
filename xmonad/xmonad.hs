-- TODO:
-- mod + enter = swap left window and next most window
--
import Data.Map.Lazy (fromList)
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves

main =
  xmonad =<< xmobar
    def
      { borderWidth = 2,
        modMask = mod4Mask,
        -- terminal = "kitty",
        terminal = "st",
        -- terminal = "urxvt",
        -- normalBorderColor = "#cccccc",
        -- focusedBorderColor = "#c62f37", -- vscode
        -- focusedBorderColor = "#fb4934", -- gruvbox
        keys = \c ->
          fromList
            [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
              ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
              ((0, xF86XK_MonBrightnessUp), spawn "light -A 5"),
              ((0, xF86XK_MonBrightnessDown), spawn "light -U 5"),
              ((mod4Mask, xK_Tab), toggleWS)
            ]
            <> keys defaultConfig c
      }
