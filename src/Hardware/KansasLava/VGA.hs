module Hardware.KansasLava.VGA( VGA(..) ) where

import Language.KansasLava
import Data.Sized.Matrix as Matrix
import Data.Sized.Unsigned as Unsigned

data VGA clk r g b =
    VGA{ vgaR :: Matrix r (Signal clk Bool)
       , vgaG :: Matrix g (Signal clk Bool)
       , vgaB :: Matrix b (Signal clk Bool)
       , vgaVSync, vgaHSync :: Signal clk Bool
       }
