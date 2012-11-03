{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Hardware.KansasLava.Boards.Papilio (
    -- * Class for the methods of the Papilio
    Papilio(..)
    -- * Initialization, and global settings.
    , clockRate
    ) where

import Language.KansasLava as KL
import Hardware.KansasLava.Rate

import Data.Sized.Ix hiding (all)
import Control.Monad.Fix

-- | The clock rate on the Papilio (32MHz), in hertz.
clockRate :: Integer
clockRate = 32 * 1000 * 1000

class MonadFix fabric => Papilio fabric where
    -- | 'board_init' sets up the use of the clock.
    -- Always call 'board_init' first. [Required].
    board_init :: fabric ()

    -- | 'tickTock' generates 'n' pulses per second,
    -- based on the expected simulation, or clockrate on the board.
    -- The purpose is for controlling real-time sampling, or for animations.
    --
    tickTock :: (Size w) => Witness w -> Integer -> fabric (Seq Bool)

instance Papilio Fabric where
  board_init = do
      -- we need to name and pull in the clock
      theClk "CLK_32MHZ"

  tickTock wit hz = return $ rate wit tickTime
    where
      clockHz = fromIntegral clockRate / fromIntegral hz
      tickTime = 1 / clockHz
