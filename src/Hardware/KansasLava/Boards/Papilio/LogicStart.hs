{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Hardware.KansasLava.Boards.Papilio.LogicStart
       ( Model(..)
         -- * Class for the methods of the Spartan3e
       , LogicStart(..)
         -- * Initialization, and global settings.
       , clockRate
       , board_init
       , toUCF
         -- * Data structures
       , Active(..)
       , SevenSegment(..)
       , Buttons(..)
       , VGA(..)
         -- -- * Utilities for Board and Simulation use
       , switchesP
         -- , buttonsP -- TODO
       , ledsP
       ) where

import Language.KansasLava as KL
import Hardware.KansasLava.Boards.Papilio
import qualified Hardware.KansasLava.Boards.Papilio.UCF as Papilio
import Hardware.KansasLava.SevenSegment
import Hardware.KansasLava.VGA

import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Control.Applicative
import Control.Monad (ap, liftM)

data Buttons clk = Buttons{ buttonUp, buttonDown
                          , buttonLeft, buttonRight
                          , buttonCenter :: Signal clk Bool
                          }

------------------------------------------------------------
-- The LogicStart class
------------------------------------------------------------

class Papilio fabric => LogicStart fabric where
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------

   switches :: fabric (Matrix X8 (Signal CLK Bool))
   buttons :: fabric (Buttons CLK)
   leds :: Matrix X8 (Signal CLK Bool) -> fabric ()
   sseg :: SevenSegment CLK ActiveLow X4 -> fabric ()
   vga :: RawVGA CLK X3 X3 X2 -> fabric ()

------------------------------------------------------------
-- initialization
------------------------------------------------------------

toUCF :: Model -> KLEG -> IO String
toUCF model = Papilio.toUCF fileName (Just "CLK_32MHZ")
  where
    fileName = "Arcade-" ++ designator ++ ".ucf"
    designator = case model of
        PapilioOne -> "One"
        PapilioPro -> "Pro"

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

  vga RawVGA{..} = do
      outStdLogicVector "VGA_R" (pack vgaRawR :: Seq (Matrix X3 Bool))
      outStdLogicVector "VGA_G" (pack vgaRawG :: Seq (Matrix X3 Bool))
      outStdLogicVector "VGA_B" (pack vgaRawB :: Seq (Matrix X2 Bool))
      outStdLogic "VGA_VSYNC" vgaRawVSync
      outStdLogic "VGA_HSYNC" vgaRawHSync

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
