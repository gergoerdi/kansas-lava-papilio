{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Hardware.KansasLava.SevenSegment
       ( Active(..)
       , SevenSegment(..)
       , decodeHexSS
       , showSS
       , driveSS
       ) where

import Language.KansasLava
import Data.Sized.Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Bits
import Control.Applicative

data Active = ActiveHigh | ActiveLow

data SevenSegment clk (active :: Active) n = SevenSegment
    { ssAnodes :: Matrix n (Signal clk Bool)
    , ssSegments :: Matrix X7 (Signal clk Bool)
    , ssDecimalPoint :: Signal clk Bool
    }

nary :: forall a clk sig n. (Clock clk, sig ~ Signal clk, Rep a, Size n, Rep n) => sig n -> Matrix n (sig a) -> sig a
nary sel inps = pack inps .!. sel

decodeHexSS :: Unsigned X4 -> Matrix X7 Bool
decodeHexSS n = matrix $ case n of
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

driveSS :: forall clk sig n. (Clock clk, sig ~ Signal clk, Size n, Rep n, Num n, Integral n) => Matrix n (Matrix X7 (sig Bool)) -> SevenSegment clk ActiveLow n
driveSS segss = SevenSegment (bitNot <$> anodes) (bitNot <$> segs) high
  where
    clkAnode :: sig Bool
    clkAnode = divideClk_ (Witness :: Witness X8)

    selector :: sig n
    selector = counter clkAnode

    segss' :: Matrix X7 (Matrix n (sig Bool))
    segss' = columns . joinRows $ segss

    segs :: Matrix X7 (sig Bool)
    segs = fmap (nary selector) segss'

    anodes :: Matrix n (sig Bool)
    anodes = fmap (.&&. clkAnode) $ rotatorL clkAnode

divideClk :: forall c sig ix. (Clock c, sig ~ Signal c, Size ix) => Witness ix -> sig Bool -> sig Bool
divideClk _ clk = counter clk .==. (0 :: sig (Unsigned ix))

divideClk_ :: forall c sig ix. (Clock c, sig ~ Signal c, Size ix) => Witness ix -> sig Bool
divideClk_ w = divideClk w high

counter :: (Rep a, Num a, Clock c, sig ~ Signal c) => sig Bool -> sig a
counter inc = loop
  where
    reg = register 0 loop
    loop = mux inc (reg, reg + 1)

rotatorL :: (Clock c, sig ~ Signal c, Size ix, Integral ix) => sig Bool -> Matrix ix (sig Bool)
rotatorL step = fromUnsigned loop
  where
    reg = register 1 loop
    loop = mux step (reg, rotateL reg 1)

fromUnsigned :: (sig ~ Signal c, Size ix) => sig (Unsigned ix) -> Matrix ix (sig Bool)
fromUnsigned = unpack . coerce Unsigned.toMatrix