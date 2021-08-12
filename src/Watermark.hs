{-# LANGUAGE ViewPatterns #-}

module Watermark
    ( recoverFingerprint
    , recoverClientID
    , watermark
    , clientIDToFingerprint
    , fingerprintToClientID
    , preprocess
    , reformat
    , hexToInteger
    , toBin
    , integerToGUID
    ) where

import           Data.Char
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord

import Watermark.Utils.Conversion
import Watermark.Utils.GUID

type GUID = String
type ClientID = GUID
type WatermarkedGUID = GUID
type Fingerprint = String
type PartialFingerprint = String

guidLength = 32
numPatterns = 4

watermark :: ClientID -> GUID -> GUID
watermark clientid (preprocess -> guid) = reformat $ zipWith processChar bitmask guid
  where
    processChar :: Char -> Char -> Char
    processChar b c | b == '0' || isDigit c = c
                    | otherwise             = toLower c
    fingerprint = clientIDToFingerprint clientid
    patternNum  = fromIntegral $ hexToInteger guid `mod` toInteger numPatterns
    bitmask     = take guidLength . drop (guidLength * patternNum) $ fingerprint

clientIDToFingerprint :: ClientID -> Fingerprint
clientIDToFingerprint = toFullLength . concatMap show . tail . toBin . hexToInteger
  where
    fullLength = guidLength * fromIntegral numPatterns
    toFullLength :: String -> String
    toFullLength b | length b <= fullLength = leftPad (fullLength - length b) b
                   | otherwise              = take fullLength b

fingerprintToClientID :: Fingerprint -> ClientID
fingerprintToClientID = reformat . integerToGUID . toDec
  where
    toDec :: String -> Integer
    toDec = L.foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0

recoverClientID :: [WatermarkedGUID] -> ClientID
recoverClientID = fingerprintToClientID . recoverFingerprint

recoverFingerprint :: [WatermarkedGUID] -> Fingerprint
recoverFingerprint (map preprocess -> guids) = concat pieces
  where
    getGroup      = (`mod` numPatterns) . hexToInteger
    guids'        = L.sortOn getGroup guids
    groups        = L.groupBy (\x y -> getGroup x == getGroup y) guids'
    partialGroups = (map . map) recoverPartial groups
    pieces        = map mergePartials partialGroups

recoverPartial :: WatermarkedGUID -> PartialFingerprint
recoverPartial [] = []
recoverPartial (g : gs) | isLower g = '1' : recoverPartial gs
                        | otherwise = '0' : recoverPartial gs

mergePartials :: [PartialFingerprint] -> Fingerprint
mergePartials ps = [ maximum (map (!! i) ps) | i <- [0 .. guidLength - 1] ]
