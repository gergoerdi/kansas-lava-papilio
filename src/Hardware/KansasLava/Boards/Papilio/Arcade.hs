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
    , PS2(..)
    ) where

import Language.KansasLava as KL
import Hardware.KansasLava.VGA
import Hardware.KansasLava.PS2
import Hardware.KansasLava.Boards.Papilio
import Hardware.KansasLava.Boards.Papilio.UCF

import Data.Sized.Ix
import Data.Sized.Matrix
import Control.Monad (ap, liftM)

data Buttons clk = Buttons{ buttonUp, buttonDown
                          , buttonLeft, buttonRight :: Signal clk Bool
                          }

class Papilio fabric => Arcade fabric where
    -- | Setup global reset signal
    wing_init :: fabric ()

    -- | Don't use this if you also use 'wing_init' as that sets the
    -- reset button as the global reset signal
    resetButton :: fabric (Signal CLK Bool)

    buttons :: fabric (Buttons CLK)
    leds :: Matrix X4 (Signal CLK Bool) -> fabric ()
    vga :: RawVGA CLK X4 X4 X4 -> fabric ()
    ps2 :: fabric (PS2 CLK, PS2 CLK)

writeUCF :: FilePath -> KLEG -> IO ()
writeUCF to = copyUCF "Arcade.ucf" to (Just "CLK_32MHZ")

instance Arcade Fabric where
    wing_init = theRst "RESET"

    resetButton = inStdLogic "RESET"

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

    ps2 = do
        ps2a <- PS2 `liftM` inStdLogic "PS2A_CLK" `ap` inStdLogic "PS2A_DAT"
        ps2b <- PS2 `liftM` inStdLogic "PS2B_CLK" `ap` inStdLogic "PS2B_DAT"
        return (ps2a, ps2b)
