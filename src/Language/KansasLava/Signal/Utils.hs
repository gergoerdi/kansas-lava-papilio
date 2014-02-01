{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.KansasLava.Signal.Utils
       ( nary
       , divideClk
       , counter
       , rotatorL
       , fromUnsigned
       ) where

import Language.KansasLava
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Bits

nary :: forall a clk sig n. (Clock clk, sig ~ Signal clk, Rep a, Size n, Rep n) => sig n -> Matrix n (sig a) -> sig a
nary sel inps = pack inps .!. sel

divideClk :: forall c sig ix. (Clock c, sig ~ Signal c, Size ix) => Witness ix -> sig Bool
divideClk _ = counter high .==. (0 :: sig (Unsigned ix))

counter :: (Rep a, Num a, Bounded a, Eq a, Clock c, sig ~ Signal c) => sig Bool -> sig a
counter inc = loop
  where
    reg = register 0 loop
    reg' = mux (reg .==. maxBound) (reg + 1, 0)
    loop = mux inc (reg, reg')

rotatorL :: (Clock c, sig ~ Signal c, Size ix, Integral ix) => sig Bool -> Matrix ix (sig Bool)
rotatorL step = fromUnsigned loop
  where
    reg = register 1 loop
    loop = mux step (reg, rotateL reg 1)

fromUnsigned :: (sig ~ Signal c, Size ix) => sig (Unsigned ix) -> Matrix ix (sig Bool)
fromUnsigned = unpack . coerce Unsigned.toMatrix
