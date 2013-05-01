-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

main = do
     session <- getEnv "DESKTOP_SESSION"
     -- xmonad  $ maybe desktopConfig desktop session
     xmonad  ( maybe desktopConfig desktop session )
		{ modMask = mod4Mask
		-- , terminal="gnome-terminal"
		}

desktop "gnome" = gnomeConfig
desktop "kde" = kde4Config
desktop "xfce" = xfceConfig
desktop "xmonad-gnome" = gnomeConfig
desktop _ = desktopConfig

