{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.KansasLava.Signal.Utils
       ( splitByte
       , debounce
       , nary
       , divideClk
       , counter
       , rotatorL
       , fromUnsigned
       , toUnsigned
       ) where

import Language.KansasLava
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned
import Data.Bits

splitByte :: (sig ~ Signal c) => sig (Unsigned X8) -> (sig (Unsigned X4), sig (Unsigned X4))
splitByte sig = (hi, lo)
  where
    mtx = fromUnsigned sig
    hi = toUnsigned . flip cropAt 4 $ mtx
    lo = toUnsigned . flip cropAt 0 $ mtx

debounce :: forall c sig n. (Clock c, sig ~ Signal c, Size n)
          => Witness n -> sig Bool -> (sig Bool, sig Bool, sig Bool)
debounce _ button = runRTL $ do
    -- Based on http://www.fpga4fun.com/Debouncer2.html
    counter <- newReg (0 :: Unsigned n)
    let counter_max = reg counter .==. maxBound
    toggle <- newReg False

    let idle = reg toggle ./=. button
        down = bitNot (reg toggle) .&&. bitNot idle .&&. counter_max
        up = reg toggle .&&. bitNot idle .&&. counter_max

    CASE [ IF idle $ do
                counter := 0
         , OTHERWISE $ do
                counter := reg counter + 1
                WHEN counter_max $ do
                    toggle := bitNot (reg toggle)
         ]

    return (up, down, reg toggle)

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

toUnsigned :: (sig ~ Signal c, Size ix) => Matrix ix (sig Bool) -> sig (Unsigned ix)
toUnsigned = coerce Unsigned.fromMatrix . pack
