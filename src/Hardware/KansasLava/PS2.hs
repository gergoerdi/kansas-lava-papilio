{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Hardware.KansasLava.PS2
       ( PS2(..)
       , samplePS2
       , decodePS2
       ) where

import Language.KansasLava
import Data.Bits
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned

data PS2 clk = PS2{ ps2Clock, ps2Data :: Signal clk Bool }

data PS2State = Idle
              | Shift
              | Parity
              | Stop
              deriving (Eq, Enum, Bounded)

instance Rep PS2State where
    type W PS2State = X2
    newtype X PS2State = XPS2State{ unXPS2State :: Maybe PS2State }

    unX = unXPS2State
    optX = XPS2State

    toRep s = toRep . optX $ s'
      where
        s' :: Maybe X4
        s' = fmap (fromIntegral . fromEnum) $ unX s

    fromRep rep = optX $ fmap (toEnum . fromIntegral) $ unX x
      where
        x :: X X4
        x = sizedFromRepToIntegral rep

    repType _ = repType (Witness :: Witness X4)

samplePS2 :: (Clock clk) => PS2 clk -> Signal clk (Enabled Bool)
samplePS2 PS2{..} = runRTL $ do
    ps2Clock' <- newReg False
    clockPattern <- newReg (0 :: U8)

    ps2Data' <- newReg False
    dataPattern <- newReg (0 :: U8)

    let fallingClock = reg ps2Clock' .&&. bitNot (var ps2Clock')

    clockPattern := (reg clockPattern `shiftL` 1) .|. unsigned ps2Clock
    CASE [ IF (reg clockPattern .==. pureS maxBound) $ do
                ps2Clock' := high
         , IF (reg clockPattern .==. pureS minBound) $ do
                ps2Clock' := low
         ]

    dataPattern := (reg dataPattern `shiftL` 1) .|. unsigned ps2Data
    CASE [ IF (reg dataPattern .==. pureS maxBound) $ do
                ps2Data' := high
         , IF (reg dataPattern .==. pureS minBound) $ do
                ps2Data' := low
         ]

    return $ packEnabled fallingClock (reg ps2Data')

decodePS2 :: (Clock clk) => Signal clk (Enabled Bool) -> Signal clk (Enabled U8)
decodePS2 line = runRTL $ do
    state <- newReg Idle
    shiftCounter <- newReg (0 :: X8)
    shift <- newReg (0 :: U8)

    parityChecked <- newReg False
    haveCode <- newReg False
    let enableOutput = var haveCode .&&. isEnabled line

    whenEnabled line $ \ps2Data -> do
        CASE
          [ IF (reg state .==. pureS Idle) $ do
                 state := mux ps2Data (pureS Shift, pureS Idle)
                 haveCode := low
                 shift := 0
          , IF (reg state .==. pureS Shift) $ do
                 let last = reg shiftCounter .==. pureS maxBound
                 state := mux last (pureS Shift, pureS Parity)
                 shiftCounter := mux last (reg shiftCounter + 1, 0)
                 shift := (reg shift `shiftR` 1) .|. (unsigned ps2Data `shiftL` 7)
          , IF (reg state .==. pureS Parity) $ do
                 state := pureS Stop
                 parityChecked := ps2Data `xor2` parity (reg shift)
          , IF (reg state .==. pureS Stop) $ do
                 state := pureS Idle
                 haveCode := ps2Data .&&. reg parityChecked
          ]

    return $ packEnabled enableOutput (reg shift)
