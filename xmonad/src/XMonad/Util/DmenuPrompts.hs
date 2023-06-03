--------------------------------------------------------------------------------
-- | Some preset functions for dmenu prompts, since I don't like XMonad.Prompt
-- and refuse to use it instead of the good ol' dmenu.
--------------------------------------------------------------------------------

module XMonad.Util.DmenuPrompts where

import XMonad hiding (workspaces)
import XMonad.StackSet (workspaces, allWindows, tag)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)

import System.IO
import System.Process (runInteractiveProcess)
import Control.Monad (when, liftM2)

getLastLineFromProcess :: MonadIO m => FilePath -> [String] -> String -> m String
getLastLineFromProcess cmd args input = io $ do
  (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
  hPutStr pin input
  hClose pin
  output <- hGetContents pout
  when (output == output) $ return ()
  hClose pout
  hClose perr
  return $ last (lines output)

menuArgs' :: String -> [String] -> [String] -> X String
menuArgs' mcmd args opts = filter (/= '\n') <$>
  getLastLineFromProcess mcmd args (unlines opts)

dmenuArgs :: [String] -> [String] -> X String
dmenuArgs = menuArgs' "dmenu"

windowsMenuArgs :: String -> [String] -> X Window
windowsMenuArgs m a = do
  ws <- gets (allWindows . windowset)
  options <- mapM (\w -> concat' (show w) <$> prettyPrint w) ws
  read . takeWhile (/= ' ') <$> menuArgs' m a options
  where concat' s1 s2 = s1 ++ " - " ++ s2
        prettyPrint w = liftM2 concat' (runQuery appName w) (runQuery title w)

workspaceMenuArgs :: String -> [String] -> X String
workspaceMenuArgs m a = do
  ws <- gets (workspaces . windowset)
  sort <- getSortByOrder
  menuArgs' m a . filter (/= "NSP") . map tag $ sort ws

windowsMenu :: String -> X Window
windowsMenu = flip windowsMenuArgs []

workspaceMenu :: String -> X String
workspaceMenu = flip workspaceMenuArgs []

workspaceDmenuArgs :: [String] -> X String
workspaceDmenuArgs = workspaceMenuArgs "dmenu"

workspaceDmenu :: X String
workspaceDmenu = workspaceDmenuArgs []
