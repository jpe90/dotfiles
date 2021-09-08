-- TODO:
-- mod + enter = swap left window and next most window
--
import Data.Map.Lazy (fromList)
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Util.Run   -- for spawnPipe and hPutStrLn

main = do
  -- xmonad =<< xmobar
  h <- spawnPipe "xmobar"
  xmonad $
    docks $ def
      { borderWidth = 2
        , modMask = mod4Mask
        -- terminal = "st",
        , terminal = "kitty"
        , manageHook=manageHook def <+> manageDocks
        , layoutHook=avoidStruts $ layoutHook def
        , logHook =  myLogHook h
        
        -- focusedBorderColor = "#7aa2f7",
        -- terminal = "st",
        -- terminal = "urxvt",
        -- normalBorderColor = "#cccccc",
        -- focusedBorderColor = "#c62f37", -- vscode
        -- focusedBorderColor = "#fb4934", -- gruvbox
        , keys = \c ->
          fromList
            [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
              ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
              ((0, xF86XK_MonBrightnessUp), spawn "light -A 5"),
              ((0, xF86XK_MonBrightnessDown), spawn "light -U 5"),
              ((mod4Mask, xK_Tab), toggleWS)
            ]
            <> keys def c
      }

myLogHook h = dynamicLogWithPP $ def
  { ppLayout = wrap "(<fc=#e4b63c>" "</fc>)"
  -- , ppSort = getSortByXineramaRule  -- Sort left/right screens on the left, non-empty workspaces after those
  , ppTitleSanitize = const ""  -- Also about window's title
  , ppVisible = wrap "(" ")"  -- Non-focused (but still visible) screen
  , ppCurrent = wrap "<fc=#b8473d>[</fc><fc=#7cac7a>" "</fc><fc=#b8473d>]</fc>"-- Non-focused (but still visible) screen
  , ppOutput = hPutStrLn h
  }

