import XMonad
import XMonad.Actions.Volume (lowerVolume, raiseVolume)
import XMonad.Util.Dzen (dzenConfig)
import XMonad.Util.EZConfig (additionalKeys)

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import Graphics.X11.ExtraTypes.XF86

alert :: (Show a) => a -> X ()
alert = dzenConfig return . show

main :: IO ()
main = xmonad $ defaultConfig
       { modMask = mod4Mask
       , startupHook = startup
       , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ layoutHook defaultConfig}
       `additionalKeys`
       [((0, xF86XK_AudioLowerVolume), lowerVolume 4 >>= alert . floor)
       ,((0, xF86XK_AudioRaiseVolume), raiseVolume 4 >>= alert . floor)]

startup :: X ()
startup = do
  spawn "xmobar"
  spawn "emacs --daemon"
        
