{-# LANGUAGE ViewPatterns #-}

module Watermark
    ( recoverFingerprint
    , recoverClientID
    , watermark
    , clientIDToFingerprint
    , fingerprintToClientID
    , preprocess
    ) where

import           Data.Char
import qualified Data.List                     as L

type ClientID = Int -- ^ Int in range [0, 2^32-1]
type GUID = String
type WatermarkedGUID = String
type Fingerprint = String
type PartialFingerprint = String

guidLength = 32

watermark :: ClientID -> GUID -> GUID
watermark clientid (preprocess -> guid) =
    let processChar :: Char -> Char -> Char
        processChar b c | b == '0' || isDigit c = c
                        | otherwise             = toLower c
    in  reformat $ zipWith processChar (clientIDToFingerprint clientid) guid

-- | Remove the hyphens from a GUID
preprocess :: GUID -> GUID
preprocess = filter (/= '-')

-- | Add the hyphens back into a GUID
reformat :: GUID -> GUID
reformat guid = L.intercalate "-" spans
  where
    spans = map
        ($ guid)
        [ take 8
        , take 4 . drop 8
        , take 4 . drop 12
        , take 4 . drop 16
        , take 4 . drop 20
        , take 8 . drop 24
        ]

clientIDToFingerprint :: ClientID -> Fingerprint
clientIDToFingerprint = to32Bits . concatMap show . tail . toBin
  where
    toBin :: Int -> [Int]
    toBin 0 = [0]
    toBin n = toBin (div n 2) ++ [mod n 2]
    to32Bits :: String -> String
    to32Bits b | length b <= guidLength = leftPad (guidLength - length b) b
               | otherwise              = take guidLength b
    leftPad :: Int -> String -> String
    leftPad 0 s = s
    leftPad n s = replicate n '0' ++ s

fingerprintToClientID :: Fingerprint -> ClientID
fingerprintToClientID = toDec
  where
    toDec :: String -> Int
    toDec = L.foldl' (\acc x -> acc * 2 + digitToInt x) 0

recoverClientID :: [WatermarkedGUID] -> ClientID
recoverClientID = fingerprintToClientID . recoverFingerprint

recoverFingerprint :: [WatermarkedGUID] -> Fingerprint
recoverFingerprint = mergePartials . map (recoverPartial . preprocess)

recoverPartial :: WatermarkedGUID -> PartialFingerprint
recoverPartial [] = []
recoverPartial (g : gs) | isLower g = '1' : recoverPartial gs
                        | otherwise = '0' : recoverPartial gs

mergePartials :: [PartialFingerprint] -> Fingerprint
mergePartials ps = [ maximum (map (!! i) ps) | i <- [0 .. guidLength - 1] ]
