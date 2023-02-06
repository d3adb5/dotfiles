module XMonad.Actions.DmenuWorkspaces
  ( selectWorkspace
  , selectWorkspace'
  , moveToWorkspace
  , moveToWorkspace'
  , renameWorkspace
  , renameWorkspace'
  , removeWorkspace
  , removeWorkspaceIfEmpty
  , removeWorkspaceWhen
  , chooseWorkspace
  , chooseWorkspace'
  ) where

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter)
import qualified XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Util.DmenuPrompts
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder, updateName, removeName)

import Control.Monad (when)
import Data.Maybe

-- | Switches to a workspace given its tag. Will create it if it doesn't exist.
goTo :: String -> X ()
goTo "" = return ()
goTo ws = do
  s <- gets windowset
  if tagMember ws s
    then windows $ greedyView ws
    else DW.addWorkspace ws

-- | Moves a window to a workspace given its tag. Will create it if it doesn't
-- exist.
sendTo :: Window -> String -> X ()
sendTo _  "" = return ()
sendTo wn ws = do
  whenX (not . tagMember ws <$> gets windowset) $
    DW.addWorkspace ws
  windows $ greedyView ws . shiftWin ws wn

-- | Prompts the user through dmenu and switches to the then chosen workspace.
selectWorkspace :: X ()
selectWorkspace = chooseWorkspace >>= goTo

selectWorkspace' :: String -> [String] -> X ()
selectWorkspace' cmd args = chooseWorkspace' cmd args >>= goTo

-- | Creates a dmenu prompt with workspace names and returns the chosen name.
chooseWorkspace :: X String
chooseWorkspace = workspaceDmenu

chooseWorkspace' :: String -> [String] -> X String
chooseWorkspace' = workspaceMenuArgs

-- | Prompts the user through dmenu and moves the given window to the chosen
-- workspace.
moveToWorkspace :: Window -> X ()
moveToWorkspace = (chooseWorkspace >>=) . sendTo

moveToWorkspace' :: String -> [String] -> Window -> X ()
moveToWorkspace' cmd args w = chooseWorkspace' cmd args >>= sendTo w

-- | Renames the current workspace by prompting the user through dmenu for a new
-- workspace tag. No entries are given, as renaming should not be picked from a
-- list of already used tags.
renameWorkspace :: X ()
renameWorkspace = dmenuArgs [] [] >>= renameWorkspaceByName

renameWorkspace' :: String -> [String] -> X ()
renameWorkspace' cmd args = menuArgs' cmd args [] >>= renameWorkspaceByName

-- | Manually updates the current workspace tag in the WindowSet with a given
-- string.
setCurrentTag :: WorkspaceId -> WindowSet -> WindowSet
setCurrentTag tag =
  let setTag wk = wk { tag = tag }
      setWsp sc = sc { workspace = setTag (workspace sc) }
      setScr ws = ws { current = setWsp (current ws) }
  in setScr

renameWorkspaceByName :: WorkspaceId -> X ()
renameWorkspaceByName "" = return ()
renameWorkspaceByName ws = do
  gets (currentTag . windowset) >>= flip updateName ws
  windows $ setCurrentTag ws . removeWorkspace' ws

removeWorkspace' :: WorkspaceId -> WindowSet -> WindowSet
removeWorkspace' wsp wset = rmWkspc xs ys
  where
    (xs, ys) = break ((== wsp) . tag) (hidden wset)

    meld (Just x) (Just y) = differentiate $ integrate x ++ integrate y
    meld a b | isNothing a = b
             | otherwise = a

    rmWkspc xs (y:ys) = wset
      { current = (current wset)
        { workspace = (workspace $ current wset)
          { stack = meld (stack y) (stack . workspace $ current wset)
          }
        }
      , hidden = xs ++ ys
      }
    rmWkspc _ _ = wset

removeWorkspaceWhen :: (WindowSpace -> Bool) -> X ()
removeWorkspaceWhen pred = do
  ws <- gets $ workspace . current . windowset
  when (pred ws) $ do
    DW.removeWorkspace
    removeName $ tag ws

removeWorkspaceIfEmpty :: X ()
removeWorkspaceIfEmpty = removeWorkspaceWhen (null . integrate' . stack)

removeWorkspace :: X ()
removeWorkspace = removeWorkspaceWhen (const True)
