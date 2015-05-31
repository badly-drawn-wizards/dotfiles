import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

main = do
     xmonad $ defaultConfig
       { modMask = mod4Mask
       , startupHook = startup
       , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ layoutHook defaultConfig}

startup :: X ()
startup = do
        spawn "xmobar"
        spawn "emacs --daemon"
        
