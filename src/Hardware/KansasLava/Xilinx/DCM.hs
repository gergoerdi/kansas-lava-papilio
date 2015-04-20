{-# LANGUAGE RecordWildCards #-}
module Hardware.KansasLava.Xilinx.DCM (dcm) where

import Language.Netlist.AST

-- | Interface to the Xilinx Digital Clock Manager module.
--
-- The DCM definition itself must be created separately in the Xilinx ISE as a
-- @.xaw@ file.
--
-- The following example creates a circuit running at 16 MHz with a native
-- clock signal of 32 MHz.
--
-- > kleg <- reifyFabric $ do
-- >     theClk "CLK_16MHZ"
-- >     fabric
-- > mod <- netlistCircuit modName kleg
-- > let mod' = dcm "dcm_32_to_16" "CLK_32MHZ" "CLK_16MHZ" mod
-- >     vhdl = genVHDL mod' ["work.lava.all", "work.all"]
-- > return vhdl
dcm :: Ident  -- ^ Name of the instantiated DCM module
    -> Ident  -- ^ @rawClock@: Name of the raw clock signal
             --   (usually, connected via the UCF to some crystal)
    -> Ident  -- ^ @newClock@: Name of the new clock signal (the output of the DCM)
    -> Module -- ^ Module using @newClock@ as its clock signal, to be wrapped
    -> Module
dcm dcmName rawClock newClock Module{..} = Module name inputs outputs [] decls
  where
    name = module_name
    inputs = (rawClock, Nothing) : filter ((/= newClock) . fst) module_inputs
    outputs = module_outputs
    decls = routing : dcmInst : module_decls

    routing = NetDecl newClock Nothing Nothing

    dcmInst = InstDecl ("work." ++ dcmName) ("inst_" ++ dcmName) []
              [ ("clkin_in",        ExprVar rawClock)
              , ("clkin_ibufg_out", open)
              ]
              [ ("clkfx_out",       ExprVar newClock)
              , ("clk0_out",        open)
              ]

    open = ExprVar "open"
