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

import           Watermark.Utils.Conversion
import           Watermark.Utils.GUID

type GUID = String
type ClientID = GUID
type WatermarkedGUID = GUID
type Fingerprint = String
type PartialFingerprint = String

guidLength = 32
numPatterns = 4

watermark :: ClientID -> GUID -> GUID
watermark (preprocess -> clientid) (preprocess -> guid) = reformat
    $ zipWith processChar bitmask guid
  where
    processChar :: Char -> Char -> Char
    processChar b c | b == '0' || isDigit c = c
                    | otherwise             = toLower c
    fingerprint = clientIDToFingerprint clientid
    patternNum  = fromIntegral $ hexToInteger guid `mod` toInteger numPatterns
    bitmask     = take guidLength . drop (guidLength * patternNum) $ fingerprint

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
