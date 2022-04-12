module Config.Layouts (layoutHook) where

import XMonad (Full(..), (|||))
import XMonad.Layout.BoringWindows
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.HiddenQueueLayout
import XMonad.Layout.MagicFocus
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing hiding (screenBorder, windowBorder)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.VoidBorders
import XMonad.Hooks.ManageDocks (avoidStruts)

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . onWorkspaces ["dev"] (threeLayout ||| centeredLayout)
  . onWorkspaces ["doc"] docLayout
  . onWorkspaces ["web", "gimp", "chat"] (fullLayout ||| threeLayout)
  $ threeLayout ||| fullLayout

threeLayout
  = avoidStruts
  . reflectHoriz
  . hideNAt 2 3
  . normalBorders
  $ ThreeColMid 1 (3/100) (1/2)

docLayout
  = ratioQueueLayout 1 (1 - D.terminalCRatio 84) D.topWindowRatio D.resizeRatio

hiddenQueueLayout
  = ratioQueueLayout 2 D.masterRatio D.topWindowRatio D.resizeRatio

ratioQueueLayout ns mr tr rr
  = avoidStruts
  . normalBorders
  . spacingRaw False (borderAll 0) True windowBorder True
  . hidden
  $ HQLayout ns mr tr rr

centeredLayout
  = avoidStruts
  . normalBorders
  . magicFocus
  . hidden
  . centerMaster
  $ Grid

fullLayout
  = voidBorders
  . avoidStruts
  . spacingRaw False (borderAll 0) True windowBorder False
  $ Full

windowBorder :: Border
windowBorder = borderAll D.windowGap

screenBorder :: Border
screenBorder = incBTop D.panelHeight $
  Border D.verticalPadding   D.verticalPadding
         D.horizontalPadding D.horizontalPadding

-- these functions should seriously be exported by X.L.Spacing

borderAll :: Integer -> Border
borderAll b = Border b b b b

incBTop :: Integer -> Border -> Border
incBTop n (Border t x y z) = Border (t + n) x y z
incBBot :: Integer -> Border -> Border
incBBot n (Border x b y z) = Border x (b + n) y z
incBRig :: Integer -> Border -> Border
incBRig n (Border x y r z) = Border x y (r + n) z
incBLef :: Integer -> Border -> Border
incBLef n (Border x y z l) = Border x y z (l + n)
incBLnR :: Integer -> Border -> Border
incBLnR n = incBRig n . incBLef n
incBTnB :: Integer -> Border -> Border
incBTnB n = incBTop n . incBBot n
incBAll :: Integer -> Border -> Border
incBAll n = incBTnB n . incBLnR n
