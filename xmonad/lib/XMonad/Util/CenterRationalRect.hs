module XMonad.Util.CenterRationalRect
  ( centerRRect
  , centerRRectOffsetX
  , centerRRectOffsetY
  , centerRRectOffsetXY
  , centerIRect
  , centerIRectOffsetX
  , centerIRectOffsetY
  , centerIRectOffsetXY
  ) where

import Data.Ratio
import XMonad.StackSet (RationalRect(..))

centerIRect :: (Integral a) => a -> a -> a -> a -> RationalRect
centerIRect = centerIRectOffsetXY 0 0

centerIRectOffsetX :: (Integral a) => a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetX ox = centerIRectOffsetXY ox 0

centerIRectOffsetY :: (Integral a) => a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetY = centerIRectOffsetXY 0

centerIRectOffsetXY :: (Integral a) => a -> a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetXY ox oy w h sw sh = centerRRectOffsetXY orx ory rw rh
  where orx = toInteger ox % toInteger sw
        ory = toInteger oy % toInteger sh
        rw = toInteger w % toInteger sw
        rh = toInteger h % toInteger sh

centerRRect :: Rational -> Rational -> RationalRect
centerRRect = centerRRectOffsetXY 0 0

centerRRectOffsetX :: Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetX ox = centerRRectOffsetXY ox 0

centerRRectOffsetY :: Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetY = centerRRectOffsetXY 0

centerRRectOffsetXY :: Rational -> Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetXY ox oy rw rh = RationalRect rx ry rw rh
  where rx = (1 - rw + ox) / 2
        ry = (1 - rh + oy) / 2
