{-# LANGUAGE OverloadedStrings #-}
module SystemInfo where

import System.Process
import qualified Data.Text.IO as TIO (readFile)

import Data.Maybe (catMaybes, fromMaybe, fromJust)

import Data.List (nub, group, sort)
import Data.Tuple.Extra (both)
import Data.List.Extra (split)

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Map as M
import Data.Map (Map)

data Interface = Interface { ifname :: String, ifaddrs :: [String] } deriving (Show)

getInterfaces :: IO [Interface]
getInterfaces =
    parseInterfaces . filter (not . null) . lines <$> readProcess "/bin/ip" ["a"] []
  where
    parseInterfaces :: [String] -> [Interface]
    parseInterfaces [] = []
    parseInterfaces input = do
        let (addressLines, remainder) = break ((' ' /=) . head) $ tail input
            interface = init $ (words $ head input) !! 1
            addresses = map (!!1) $ filter (("inet" ==) . head) $ map words addressLines
        (Interface interface addresses):(parseInterfaces remainder)

enumerateAddresses :: [Interface] -> [(String, String)]
enumerateAddresses = concat . map go
  where go iface = map ((,) $ ifname iface) $ ifaddrs iface

isLoopback :: Interface -> Bool
isLoopback (Interface name _) = take 2 name == "lo"

getMemoryKibibytes, getMemoryGibibytes :: IO Double
getMemoryKibibytes = do
    meminfo <- readFile "/proc/meminfo"
    let kibibytes = head . tail . words . head . lines $ meminfo
    return (read kibibytes :: Double)

getMemoryGibibytes = (\m -> m * 1024 / 1073741824) <$> getMemoryKibibytes

getUname :: IO Text
getUname = T.strip . T.pack <$> readProcess "/bin/uname" ["-nsrm"] []

getCpuInfo :: IO [Text]
getCpuInfo = do
    cpuinfo <- TIO.readFile "/proc/cpuinfo"
    let cpus = split T.null . T.lines . T.init $ cpuinfo
        cpus' = uniqCpus $ map parseCpu cpus
    return $ uniqNames cpus'
  where
    parseCpu :: [Text] -> Map Text Text
    parseCpu lines = do
        M.fromList $ map (strip . T.break (':'==)) lines
      where
        strip (k,v) = both T.strip (k, T.drop 1 v)

    uniqCpus :: [Map Text Text] -> [Map Text Text]
    uniqCpus cpus = do
        let pids = nub $ map lookupPid cpus
        map (\pid -> head $ filter ((== pid) . lookupPid) cpus) pids
      where
        lookupPid :: Map Text Text -> Text
        lookupPid = fromJust . M.lookup "physical id"

    uniqNames :: [Map Text Text] -> [Text]
    uniqNames cpus = do
        let names = map (fromJust . M.lookup "model name") cpus
            names' = map (\g -> (length g, head g)) (group . sort $ names)
        map toText names'
      where
        toText :: (Int, Text) -> Text
        toText (1, name) = name
        toText (n, name) = (T.pack . show) n <> "x " <> name
