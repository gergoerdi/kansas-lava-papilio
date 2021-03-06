{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Hardware.KansasLava.SevenSegment
       ( Active(..)
       , SevenSegment(..)
       , encodeHexSS
       , showSS
       , driveSS
       , driveSS_
       ) where

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Maybe (isJust, fromMaybe)
import Control.Applicative

data Active = ActiveHigh | ActiveLow

data SevenSegment clk (active :: Active) n = SevenSegment
    { ssAnodes :: Matrix n (Signal clk Bool)
    , ssSegments :: Matrix X7 (Signal clk Bool)
    , ssDecimalPoint :: Signal clk Bool
    }

encodeHexSS :: Unsigned X4 -> Matrix X7 Bool
encodeHexSS n = matrix $ case n of
    --        a      b      c      d      e      f      g
    0x0 -> [  True,  True,  True,  True,  True,  True, False ]
    0x1 -> [ False,  True,  True, False, False, False, False ]
    0x2 -> [  True,  True, False,  True,  True, False,  True ]
    0x3 -> [  True,  True,  True,  True, False, False,  True ]
    0x4 -> [ False,  True,  True, False, False,  True,  True ]
    0x5 -> [  True, False,  True,  True, False,  True,  True ]
    0x6 -> [  True, False,  True,  True,  True,  True,  True ]
    0x7 -> [  True,  True,  True, False, False, False, False ]
    0x8 -> [  True,  True,  True,  True,  True,  True,  True ]
    0x9 -> [  True,  True,  True,  True, False,  True,  True ]
    0xa -> [  True,  True,  True, False,  True,  True,  True ]
    0xb -> [ False, False,  True,  True,  True,  True,  True ]
    0xc -> [  True, False, False,  True,  True,  True, False ]
    0xd -> [ False,  True,  True,  True,  True, False,  True ]
    0xe -> [  True, False, False,  True,  True,  True,  True ]
    0xf -> [  True, False, False, False,  True,  True,  True ]

-- For testing
showSS :: Matrix X7 Bool -> String
showSS (toList -> [a, b, c, d, e, f, g])
  = unlines
    [ vpad   ++ horiz a ++ vpad
    , vert f ++ hpad    ++ vert b
    , vert f ++ hpad    ++ vert b
    , vpad   ++ horiz g ++ vpad
    , vert e ++ hpad    ++ vert c
    , vert e ++ hpad    ++ vert c
    , vpad   ++ horiz d ++ vpad
    ]
  where
    vpad = replicate 1 ' '
    hpad = replicate 3 ' '
    horiz b = replicate 3 $ if b then '#' else ' '
    vert b = replicate 1 $ if b then '#' else ' '

driveSS_ :: forall clk sig n. (Clock clk, sig ~ Signal clk, Size n, Rep n, Num n, Integral n)
         => Matrix n (Maybe (Matrix X7 (sig Bool)))
         -> SevenSegment clk ActiveLow n
driveSS_ segss = driveSS mask segss'
  where
    mask = fmap (pureS . isJust) segss
    segss' = fmap (fromMaybe noSegs) segss

    noSegs :: Matrix X7 (sig Bool)
    noSegs = matrix $ replicate 7 low

driveSS :: forall clk sig n. (Clock clk, sig ~ Signal clk, Size n, Rep n, Num n, Integral n)
        => Matrix n (sig Bool)
        -> Matrix n (Matrix X7 (sig Bool))
        -> SevenSegment clk ActiveLow n
driveSS mask segss = SevenSegment (bitNot <$> anodes') (bitNot <$> segs) high
  where
    clkAnode :: sig Bool
    clkAnode = divideClk (Witness :: Witness X4)

    selector :: sig n
    selector = counter clkAnode

    segss' :: Matrix X7 (Matrix n (sig Bool))
    segss' = columns . joinRows $ segss

    segs :: Matrix X7 (sig Bool)
    segs = fmap (nary selector) segss'

    anodes :: Matrix n (sig Bool)
    anodes = fmap (.&&. clkAnode) $ rotatorL clkAnode

    anodes' :: Matrix n (sig Bool)
    anodes' = Matrix.zipWith (.&&.) mask anodes
