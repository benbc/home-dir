import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Spiral
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
import System.IO

myManageHook = composeAll
               [ className =? "Gimp"      --> doFloat
               , className =? "Vncviewer" --> doFloat
               ] <+> manageDocks
               <+> scratchpadManageHook (W.RationalRect 0.25 0.1 0.5 0.8)
               <+> manageHook defaultConfig

myLayoutHook = avoidStruts $ Tall 1 (1/100) (1/2) ||| Full ||| Mirror (Tall 2 (3/100) (3/4))

myConfig xmproc = defaultConfig
                  { manageHook = myManageHook
                  , layoutHook = myLayoutHook
                  , logHook = dynamicLogWithPP $ xmobarPP
                              { ppOutput = hPutStrLn xmproc
                              , ppTitle = xmobarColor "green" "" . shorten 50
                              }
                  , modMask = modm
                  , terminal = "urxvtcd"
                  , startupHook = setWMName "LG3D"
                  , focusFollowsMouse = False
                  } `additionalKeys`
                  [ ((modm, xK_s), scratchpadSpawnActionTerminal "urxvtcd") ]
    where modm = mod4Mask

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad (myConfig xmproc)
