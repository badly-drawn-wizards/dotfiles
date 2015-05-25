import XMonad
import XMonad.Hooks.ManageDocks

main = do
     xmonad $ defaultConfig
       { modMask = mod4Mask
       , startupHook = startup
       , manageHook = manageDocks <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ layoutHook defaultConfig }

startup :: X ()
startup = do
        spawn "xmobar"
        spawn "emacs --daemon"
