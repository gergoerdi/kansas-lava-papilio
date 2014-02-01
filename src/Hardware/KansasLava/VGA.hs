{-# LANGUAGE RecordWildCards #-}
module Hardware.KansasLava.VGA
       ( VGA(..), RawVGA(..)
       , encodeVGA
       ) where

import Language.KansasLava
import Language.KansasLava.Signal.Utils
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned

data VGA clk r g b =
    VGA{ vgaR :: Signal clk (Unsigned r)
       , vgaG :: Signal clk (Unsigned g)
       , vgaB :: Signal clk (Unsigned b)
       , vgaVSync, vgaHSync :: Signal clk Bool
       }

data RawVGA clk r g b =
    RawVGA{ vgaRawR :: Matrix r (Signal clk Bool)
          , vgaRawG :: Matrix g (Signal clk Bool)
          , vgaRawB :: Matrix b (Signal clk Bool)
          , vgaRawVSync, vgaRawHSync :: Signal clk Bool
          }

encodeVGA :: (Size r, Size g, Size b) => VGA clk r g b -> RawVGA clk r g b
encodeVGA VGA{..} = RawVGA{..}
  where
    vgaRawR = fromUnsigned vgaR
    vgaRawG = fromUnsigned vgaG
    vgaRawB = fromUnsigned vgaB

    vgaRawVSync = vgaVSync
    vgaRawHSync = vgaHSync
