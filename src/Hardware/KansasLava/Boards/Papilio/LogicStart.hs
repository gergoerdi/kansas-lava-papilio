{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Hardware.KansasLava.Boards.Papilio.LogicStart (
    -- * Class for the methods of the Spartan3e
    LogicStart(..)
    -- * Initialization, and global settings.
    , clockRate
    , board_init
    , writeUCF
      -- * Data structures
    , Active(..)
    , SevenSegment(..)
    , Buttons(..)
      -- -- * Utilities for Board and Simulation use
    , switchesP
      -- , buttonsP -- TODO
    , ledsP
    ) where

import Language.KansasLava as KL
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.UCF
import Hardware.KansasLava.SevenSegment

import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Control.Applicative
import Control.Monad (ap, liftM)

data Buttons = Buttons
    { buttonUp, buttonDown
    , buttonLeft, buttonRight
    , buttonCenter :: Seq Bool
    }

------------------------------------------------------------
-- The LogicStart class
------------------------------------------------------------

class Papilio fabric => LogicStart fabric where
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------

   switches :: fabric (Matrix X8 (Seq Bool))
   buttons :: fabric Buttons
   leds :: Matrix X8 (Seq Bool) -> fabric ()
   sseg :: SevenSegment CLK ActiveLow X4 -> fabric ()

------------------------------------------------------------
-- initialization
------------------------------------------------------------

-- | show out a suggested UCF file for the LogicStart, for a specific circuit.
writeUCF :: FilePath -> KLEG -> IO ()
writeUCF = copyUCF "LogicStart.ucf"

------------------------------------------------------------
-- instance
------------------------------------------------------------

instance LogicStart Fabric where
  ------------------------------------------------------------
  -- RAW APIs
  ------------------------------------------------------------

  switches = do
        inp <- inStdLogicVector "SWITCH" :: Fabric (Seq (Matrix X8 Bool))
        return (unpack inp)

  buttons = Buttons
            `liftM` inStdLogic "BTN_UP"
            `ap`    inStdLogic "BTN_DOWN"
            `ap`    inStdLogic "BTN_LEFT"
            `ap`    inStdLogic "BTN_RIGHT"
            `ap`    inStdLogic "BTN_CENTER"

  leds inp = outStdLogicVector "LED" (pack inp :: Seq (Matrix X8 Bool))

  sseg SevenSegment{..} = do
      outStdLogicVector "SS_ANODES" (pack ssAnodes :: Seq (Matrix X4 Bool))
      outStdLogicVector "SS_SEGS" (pack ssSegments :: Seq (Matrix X7 Bool))
      outStdLogic "SS_DP" ssDecimalPoint

-------------------------------------------------------------
-- Utilites that can be shared
-------------------------------------------------------------

-- | 'switchesP' gives a patch-level API for the toggle switches.
switchesP :: (LogicStart fabric)
          => fabric (Patch () (Matrix X8 (Seq Bool)) () (Matrix X8 ()))
switchesP = do
    sws <- switches
    return $
      outputP sws $$
      backwardP (\ _mat -> ()) $$
      matrixStackP (pure emptyP)

{-
-- | 'buttonsP' gives a patch-level API for the toggle switches.
buttonsP :: (LogicStart fabric)
         => fabric (Patch () Buttons () (Matrix X5 ()))
buttonsP = do
    btns <- buttons
    return $
      outputP btns $$
      backwardP (\ _mat -> ()) $$
      matrixStackP (pure emptyP)
-}

-- | 'ledP' gives a patch-level API for the leds.
ledsP :: (LogicStart fabric)
      => Patch (Matrix X8 (Seq Bool)) (fabric ()) (Matrix X8 ()) ()
ledsP =
    backwardP (\ () -> pure ()) $$
    forwardP leds
