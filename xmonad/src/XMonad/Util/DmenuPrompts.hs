--------------------------------------------------------------------------------
-- | Some preset functions for dmenu prompts, since I don't like XMonad.Prompt
-- and refuse to use it instead of the good ol' dmenu.
--------------------------------------------------------------------------------

module XMonad.Util.DmenuPrompts
  ( menuArgs'
  , dmenuArgs
  , windowsMenuArgs
  , windowsMenu
  , workspaceMenuArgs
  , workspaceMenu
  , workspaceDmenuArgs
  , workspaceDmenu
  ) where

import XMonad hiding (workspaces)
import XMonad.StackSet (workspaces, allWindows, tag)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)

import System.IO
import System.Process (runInteractiveProcess)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.List.Split (splitOneOf)

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
  options <- mapM windowPrettyPrint ws
  read . takeWhile (/= ' ') <$> menuArgs' m a options

-- Lambda lifted way to turn a Window into a String for menu indexing.
windowPrettyPrint :: Window -> X String
windowPrettyPrint w = foldl optionConcat (show w) <$> mapM (`runQuery` w) windowQueries

-- Lambda lifted way to concatenate two strings into a menu option.
optionConcat :: String -> String -> String
optionConcat s1 s2 = s1 ++ " • " ++ s2

-- List of queries to run for a Window when listing it as a menu option.
windowQueries :: [Query String]
windowQueries = [titlefy <$> className, titlefy <$> appName, title]
  where titlefy = unwords . map capitalize . splitOneOf " -_"
        capitalize (h:t) = toUpper h : t
        capitalize [] = []

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
