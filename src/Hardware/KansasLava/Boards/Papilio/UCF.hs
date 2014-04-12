module Hardware.KansasLava.Boards.Papilio.UCF (copyUCF) where

import Hardware.KansasLava.Boards.UCF (copyUCFFrom)
import Language.KansasLava
import System.FilePath.Posix ((</>))

import Paths_kansas_lava_papilio

copyUCF :: FilePath -> Maybe String -> FilePath -> KLEG -> IO ()
copyUCF fileName rawClock dest kleg = do
    src <- getDataFileName ("ucf" </> fileName)
    copyUCFFrom src rawClock dest kleg
