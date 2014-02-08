module Hardware.KansasLava.Boards.Papilio.UCF (
      filterUCF
    , copyUCFFrom
    , copyUCF
    ) where

import Language.KansasLava as KL
import Data.Char
import Data.Maybe (maybeToList)
import System.FilePath.Posix ((</>))

import Paths_kansas_lava_papilio

filterUCF :: Maybe String -> KLEG -> String -> String
filterUCF rawClock kleg ucf = unlines (hdr ++ lns)
  where
    hdr = [ "# Generated automatically by kansas-lava-cores"
          , "#" ++ show portsUsed
          ]

    lns = [ commentUnless (allowLine ln) ln | ln <- lines ucf ]

    allowLine s = case getName s of
        Nothing -> True
        Just nm -> nm `elem` portsUsed

    portsUsed = maybeToList rawClock ++ concatMap (uncurry portsFrom) ports
      where
        ports = inputs ++ outputs
        inputs = theSrcs kleg
        outputs = map (\ (a,b,_) -> (a,b)) $ theSinks kleg

    getName xs
      | take 5 xs == "NET \"" = Just (takeWhile (/= '"') (drop 5 xs))
      | otherwise = Nothing

commentUnless :: Bool -> String -> String
commentUnless True ln = ln
commentUnless False ln = "# -- " ++ ln

_isComment :: String -> Bool
_isComment ('#':_) = True
_isComment xs | all isSpace xs = True
              | otherwise = False

portsFrom nm ty = case toStdLogicType ty of
    SL -> [ nm ]
    SLV n -> [ leg i | i <- [0..(n-1)] ]
  where
    leg i = nm ++ "<" ++ show i ++ ">"

copyUCFFrom :: FilePath -> FilePath -> Maybe String -> KLEG -> IO ()
copyUCFFrom src dest rawClock kleg = do
    big_ucf <- readFile src
    let small_ucf = filterUCF rawClock kleg big_ucf
    writeFile dest small_ucf

copyUCF :: FilePath -> FilePath -> Maybe String -> KLEG -> IO ()
copyUCF fileName dest rawClock kleg = do
    src <- getDataFileName ("ucf" </> fileName)
    copyUCFFrom src dest rawClock kleg
