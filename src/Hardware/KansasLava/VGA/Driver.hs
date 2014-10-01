{-# LANGUAGE RecordWildCards #-}
module Hardware.KansasLava.VGA.Driver
       ( -- * Generic VGA driver
         VGAParams(..)
       , VGATiming(..)
       , VGADriverIn(..)
       , VGADriverOut(..)
       , driveVGA
         -- * Timing parameters for predefined VGA modes
       , vga640x480at60
       , vga800x600at72
       , vga800x600at60
       ) where

import Language.KansasLava
import Hardware.KansasLava.VGA as VGA

import Data.Sized.Unsigned as Unsigned
import Data.Sized.Ix

data VGADriverIn clk r g b = VGADriverIn
                             { vgaInR :: Signal clk r
                             , vgaInG :: Signal clk g
                             , vgaInB :: Signal clk b
                             }

data VGADriverOut clk w h r g b = VGADriverOut
                                  { vgaOut :: VGA clk r g b
                                  , vgaOutClkPhase :: Signal clk Bool
                                  , vgaOutVBlank :: Signal clk Bool
                                  , vgaOutX :: Signal clk (Enabled (Unsigned w))
                                  , vgaOutY :: Signal clk (Enabled (Unsigned h))
                                  }

data VGAParams w h = VGAParams
                     { vgaHorizTiming :: VGATiming w
                     , vgaVertTiming :: VGATiming h
                     }

data VGATiming a = VGATiming{ visibleSize, pre, syncPulse, post :: Unsigned a }

-- | Assumes a circuit clock at double the frequency of the pixel clock
driveVGA :: (Clock clk, Rep r, Rep g, Rep b, Size w, Size h)
         => VGAParams w h
         -> VGADriverIn clk r g b
         -> VGADriverOut clk w h r g b
driveVGA VGAParams{..} VGADriverIn{..} = runRTL $ do
    hCount <- newReg 0
    vCount <- newReg 0

    let hEnd = reg hCount .==. pureS hMax
        vEnd = reg vCount .==. pureS vMax

    let phase = iterateS bitNot False
    WHEN phase $ do
        hCount := mux hEnd (reg hCount + 1, 0)
        WHEN hEnd $ do
            vCount := mux vEnd (reg vCount + 1, 0)

    let hsync = pureS hSyncStart .<=. reg hCount .&&.
                reg hCount .<. pureS hSyncEnd
        vsync = pureS vSyncStart .<=. reg vCount .&&.
                reg vCount .<. pureS vSyncEnd

    let hVisible = reg hCount .<. pureS hSize
        vVisible = reg vCount .<. pureS vSize
        visible = hVisible .&&. vVisible

    let vgaOutClkPhase = phase
        vgaOutVBlank = phase .&&. reg hCount .==. 0 .&&.
                       reg vCount .==. pureS vSyncStart
        vgaOutX = packEnabled visible (reg hCount)
        vgaOutY = packEnabled visible (reg vCount)
        vgaOut = VGA{ vgaR = packEnabled visible vgaInR
                    , vgaG = packEnabled visible vgaInG
                    , vgaB = packEnabled visible vgaInB
                    , vgaHSync = bitNot hsync
                    , vgaVSync = bitNot vsync
                    }

    return VGADriverOut{..}
  where
    hSize = visibleSize vgaHorizTiming
    hPre = pre vgaHorizTiming
    hSync = syncPulse vgaHorizTiming
    hSyncStart = hSize + hPre
    hSyncEnd = hSyncStart + hSync
    hPost = post vgaHorizTiming
    hMax = sum [hSize, hPre, hSync, hPost] - 1

    vSize = visibleSize vgaVertTiming
    vPre = pre vgaVertTiming
    vSync = syncPulse vgaVertTiming
    vSyncStart = vSize + vPre
    vSyncEnd = vSyncStart + vSync
    vPost = post vgaVertTiming
    vMax = sum [vSize, vPre, vSync, vPost] - 1

-- | VGA 640*480@60Hz, 25.175 MHz pixel clock
vga640x480at60 :: VGAParams X10 X10
vga640x480at60 = VGAParams{ vgaHorizTiming = VGATiming 640 16 96 48
                          , vgaVertTiming  = VGATiming 480 10  2 33
                          }

-- | VGA 800x600@72Hz, 50 MHz pixel clock
vga800x600at72 :: VGAParams X11 X10
vga800x600at72 = VGAParams{ vgaHorizTiming = VGATiming 800 56 120 64
                          , vgaVertTiming  = VGATiming 600 37   6 23
                          }

-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga800x600at60 :: VGAParams X11 X10
vga800x600at60 = VGAParams{ vgaHorizTiming = VGATiming 800 40 128 88
                          , vgaVertTiming  = VGATiming 600  1   4 23
                          }
