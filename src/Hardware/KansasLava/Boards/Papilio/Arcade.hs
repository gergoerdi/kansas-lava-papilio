{-# LANGUAGE RecordWildCards #-}
module Hardware.KansasLava.Boards.Papilio.Arcade (
    -- * Class for the methods of the Spartan3e
    Arcade(..)
    -- * Initialization, and global settings.
    , clockRate
    , board_init
    , writeUCF
      -- * Data structures
    , Buttons(..)
    , RawVGA(..)
    ) where

import Language.KansasLava as KL
import Hardware.KansasLava.VGA
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.UCF

import Data.Sized.Ix
import Data.Sized.Matrix
import Control.Monad (ap, liftM)

data Buttons = Buttons{ buttonUp, buttonDown
                      , buttonLeft, buttonRight :: Seq Bool
                      }

class Papilio fabric => Arcade fabric where
   resetButton :: fabric (Seq Bool)
   buttons :: fabric Buttons
   leds :: Matrix X4 (Seq Bool) -> fabric ()
   vga :: RawVGA CLK X4 X4 X4 -> fabric ()

writeUCF :: FilePath -> KLEG -> IO ()
writeUCF = copyUCF "Arcade.ucf"

instance Arcade Fabric where
  resetButton = inStdLogic "BTN_RESET"

  buttons = Buttons
            `liftM` inStdLogic "BTN_UP"
            `ap`    inStdLogic "BTN_DOWN"
            `ap`    inStdLogic "BTN_LEFT"
            `ap`    inStdLogic "BTN_RIGHT"

  leds inp = outStdLogicVector "LED" (pack inp :: Seq (Matrix X4 Bool))

  vga RawVGA{..} = do
      outStdLogicVector "VGA_R" (pack vgaRawR :: Seq (Matrix X4 Bool))
      outStdLogicVector "VGA_G" (pack vgaRawG :: Seq (Matrix X4 Bool))
      outStdLogicVector "VGA_B" (pack vgaRawB :: Seq (Matrix X4 Bool))
      outStdLogic "VGA_VSYNC" vgaRawVSync
      outStdLogic "VGA_HSYNC" vgaRawHSync
