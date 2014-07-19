module Hardware.KansasLava.Boards.Papilio.UCF (toUCF) where

import Language.KansasLava
import Hardware.KansasLava.Boards.UCF (filterUCF)
import System.FilePath.Posix ((</>))

import Paths_kansas_lava_papilio

getUCF :: FilePath -> IO String
getUCF fileName = getDataFileName ("ucf" </> fileName)

toUCF :: FilePath -> Maybe String -> KLEG -> IO String
toUCF fileName rawClock kleg = do
    src <- readFile =<< getUCF fileName
    return $ filterUCF rawClock kleg src
