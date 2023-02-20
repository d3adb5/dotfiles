{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module XMonad.Layout.Hidden
  ( hidden
  , hideNAt
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Hidden
import XMonad.Util.Hidden
import XMonad.Util.ExtensibleState as XS
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.BoringWindows as BW

import Control.Monad (when)
import Data.Foldable (toList)
import Data.Sequence ((<|))

data HiddenL a = HiddenL         deriving (Read, Show)
data HideNAt a = HideNAt Int Int deriving (Read, Show)

hidden :: l Window -> ModifiedLayout HiddenL l Window
hidden = ModifiedLayout HiddenL

hideNAt :: Int -> Int -> l Window -> ModifiedLayout HiddenL (ModifiedLayout HideNAt l) Window
hideNAt n at = hidden . ModifiedLayout (HideNAt n at)

instance LayoutModifier HiddenL Window where
  modifierDescription = const "Hidden"

  modifyLayout HiddenL wksp rect = do
    allHidden <- XS.gets hiddenWindows
    let filtered = W.stack wksp >>= W.filter (`notElem` allHidden)
    runLayout (wksp {W.stack = filtered}) rect

  handleMess HiddenL m
    | Just BW.UpdateBoring <- fromMessage m = do
        hiddenW <- XS.gets hiddenWindows
        ws <- XMonad.gets (W.workspace . W.current . windowset)
        flip sendMessageWithNoRefresh ws $ BW.Replace "Hidden" (toList hiddenW)
        return Nothing
    | otherwise = return Nothing

instance LayoutModifier HideNAt Window where
  modifierDescription (HideNAt n at) = "Hide " ++ show n ++ " at " ++ show at

  modifyLayout (HideNAt n at) wksp rect = do
    let visibleWindows = W.integrate' (W.stack wksp)
    hiddenW <- getHidden

    let willHideWindow = length visibleWindows > at
        willUnhideWindow = length hiddenW > 1 && length visibleWindows < at
        stackToBeTiled | willHideWindow = deleteNthElem n (W.stack wksp)
                       | willUnhideWindow = prepend (rightmost hiddenW) (W.stack wksp)
                       | otherwise = W.stack wksp

    when willUnhideWindow $ unhideWindowAndAct (rightmost hiddenW) (pure ())
    when willHideWindow $ hideWindowAndAct (<|) (visibleWindows !! (n - 1)) (pure ())
    runLayout (wksp { W.stack = stackToBeTiled }) rect

deleteNthElem :: Int -> Maybe (W.Stack a) -> Maybe (W.Stack a)
deleteNthElem n s =
  let listFromStack = W.integrate' s
      (start, end) = splitAt n listFromStack
      nRemoved = take (n - 1) start ++ end
  in W.differentiate nRemoved

prepend :: a -> Maybe (W.Stack a) -> Maybe (W.Stack a)
prepend w s =
  let listFromStack = W.integrate' s
  in W.differentiate (w : listFromStack)
