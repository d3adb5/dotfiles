module Config.Layouts (layoutHook) where

import XMonad (Full(..), (|||))
import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing hiding (windowBorder)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.VoidBorders
import XMonad.Hooks.ManageDocks (avoidStruts)

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . onWorkspaces ["chat"] (gridLayout ||| fullLayout)
  . onWorkspaces ["gimp"] (fullLayout ||| threeLayout)
  $ threeLayout ||| fullLayout

gridLayout
  = avoidStruts
  . normalBorders
  $ Grid

threeLayout
  = avoidStruts
  . reflectHoriz
  . hideNAt 2 3
  . normalBorders
  $ ThreeColMid 1 (3/100) (1/2)

fullLayout
  = voidBorders
  . avoidStruts
  . spacingRaw False (borderAll 0) True windowBorder False
  $ Full

windowBorder :: Border
windowBorder = borderAll D.windowGap

borderAll :: Integer -> Border
borderAll b = Border b b b b
