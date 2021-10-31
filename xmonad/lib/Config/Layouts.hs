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
import XMonad.Layout.Spacing hiding (screenBorder, windowBorder)
import XMonad.Layout.ZeroBorders
import XMonad.Hooks.ManageDocks (avoidStruts)

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . onWorkspaces ["dev"] centeredLayout
  . onWorkspaces ["doc"] docLayout
  . onWorkspaces ["web", "gimp", "chat"] (fullLayout ||| hiddenQueueLayout)
  $ hiddenQueueLayout ||| fullLayout

docLayout
  = ratioQueueLayout 1 (1 - D.terminalCRatio 84) D.topWindowRatio D.resizeRatio

hiddenQueueLayout
  = ratioQueueLayout 2 D.masterRatio D.topWindowRatio D.resizeRatio

ratioQueueLayout ns mr tr rr
  = avoidStruts
  . spacingRaw False (borderAll 0) True windowBorder True
  . hidden
  $ HQLayout ns mr tr rr

centeredLayout
  = avoidStruts
  . magicFocus
  . hidden
  . centerMaster
  $ Grid

fullLayout
  = zeroBorders
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
