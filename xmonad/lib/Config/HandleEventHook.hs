module Config.HandleEventHook (handleEventHook) where

import XMonad hiding (handleEventHook)
import XMonad.Hooks.ManageDocks (docksEventHook)
import XMonad.Layout.MagicFocus (followOnlyIf, disableFollowOnWS)

import Data.Semigroup (All(..))

handleEventHook :: Event -> X All
handleEventHook
    = docksEventHook
  <+> followOnlyIf (disableFollowOnWS ["dev"])
  <+> defaultHEHook

defaultHEHook :: Event -> X All
defaultHEHook = const $ return (All True)
