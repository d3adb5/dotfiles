module Config.LogHook (logHook) where

import XMonad hiding (logHook)
import XMonad.Actions.Hidden (unhideOnFocus)
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Util.Hidden (withHidden)
import XMonad.Util.SpawnNamedPipe

import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.DynamicWorkspaces as DW

import Control.Monad (when)
import Data.Maybe (isNothing)
import System.IO (hPutStrLn)
import Text.Printf

barPP :: PP
barPP = def
  { ppHidden = const "", ppHiddenNoWindows = const "", ppVisible = const ""
  , ppUrgent = const ""
  , ppOrder = \(ws:_:_:rs) -> ws:rs
  , ppSep = " "
  , ppCurrent = id
  , ppExtras = [ hiddenNum ]
  }
  where
    hiddenNum = withHidden $ return . catchZero . length
    catchZero l = if l == 0 then Nothing else Just $ replicate l '―'

logHook :: X ()
logHook = do
  Just xmobarHandle <- getNamedPipe "xmopipe"
  unhideOnFocus
  updatePointer (0.5, 0.5) (0.5, 0.5)
  removeWhenEmpty ["gimp", "mpv", "osu"]
  avoidWorkspaces ["NSP"]
  dynamicLogWithPP $
    barPP { ppOutput = hPutStrLn xmobarHandle . centerField . words }
  where
    centerField [] = error "centerField: empty list"
    centerField (x:xs) = printf "%s %s %s" (concat xs) x (concat xs)

avoidWorkspaces :: [WorkspaceId] -> X ()
avoidWorkspaces wss = withWorkspace $ \ws ->
  when (W.tag ws `elem` wss) $
    DO.withNthWorkspace' (filter (`notElem` wss)) W.greedyView 0

removeWhenEmpty :: [WorkspaceId] -> X ()
removeWhenEmpty wss = withWorkspace $ \ws ->
  when (isNothing (W.stack ws) && W.tag ws `elem` wss) $
    DW.removeWorkspaceByTag $ W.tag ws

withWorkspace :: (WindowSpace -> X a) -> X a
withWorkspace = (gets (W.workspace . W.current . windowset) >>=)
