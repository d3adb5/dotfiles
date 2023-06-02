module Config.Bindings (keyBindings, mouseBindings, removedBindings) where

import XMonad hiding (mouseResizeWindow, mouseBindings)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.DmenuWorkspaces
import XMonad.Actions.DynamicWorkspaceOrder (withNthWorkspace')
import XMonad.Actions.FlexibleResize (mouseResizeWindow)
import XMonad.Actions.FloatKeys (keysMoveWindowTo)
import XMonad.Actions.Hidden
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.BoringWindows (focusDown, focusUp)
import XMonad.Util.NamedScratchpad

import qualified Config.ManageHook as MH
import qualified XMonad.StackSet as W

keyBindings :: [(String, X ())]
keyBindings =
  [ ("M-]", spawn =<< fmap (terminal . config) ask)
  , ("M-j", focusDown)
  , ("M-k", focusUp)
  , ("M-<R>", moveTo Next (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-<L>", moveTo Prev (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-y", selectWorkspace' "fzfmenu" fzfmenuArgs)
  , ("M-u", renameWorkspace' "fzfmenu" fzfmenuArgs)
  , ("M-i", removeWorkspaceIfEmpty)
  , ("M-M1-h", withFocused hideWindow)
  , ("M-M1-j", withFocused swapWithNextHidden)
  , ("M-M1-k", withFocused swapWithLastHidden)
  , ("M-M1-l", withLastHidden unhideWindow)
  , ("M-[", namedScratchpadAction MH.scratchpads "term")
  , ("M-s v", namedScratchpadAction MH.scratchpads "volume")
  , ("M-C-y", chooseWorkspace >>= windows . copy)
  , ("M-<Backspace>", kill1)
  , ("M-S-y", withFocused $ moveToWorkspace' "fzfmenu" fzfmenuArgs)
  , ("M-M1-c", withFocused $ keysMoveWindowTo (681,392) (1/2,1/2))
  ] ++ [ ("M-" ++ show n, withNthWorkspace' notNSP W.greedyView (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-C-" ++ show n, withNthWorkspace' notNSP copy (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-S-" ++ show n, withNthWorkspace' notNSP W.shift (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-" ++ key, viewScreen def sc)
         | (key, sc) <- zip ["w", "e", "r"] [0..] ]
    ++ [ ("M-S-" ++ key, sendToScreen def sc)
         | (key, sc) <- zip ["w", "e", "r"] [0..] ]

removedBindings :: [String]
removedBindings = [ "M-p", "M-S-r" ]

mouseBindings :: [((ButtonMask, Button), Window -> X ())]
mouseBindings = [((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w)]

notNSP :: [WorkspaceId] -> [WorkspaceId]
notNSP = filter (/= "NSP")

fzfmenuArgs :: [String]
fzfmenuArgs = [ "--print-query", "--reverse", "+m" ]
