{-# LANGUAGE ViewPatterns #-}

module Watermark
    ( recoverFingerprint
    , recoverClientID
    , watermark
    , clientIDToFingerprint
    , fingerprintToClientID
    , GUID.preprocess
    , GUID.reformat
    , hexToInteger
    , toBin
    , integerToGUID
    ) where

import           Data.Char                      (intToDigit, toUpper)
import qualified Data.List                     as L

import           Watermark.Utils.Conversion
import qualified Watermark.Utils.GUID          as GUID

type WatermarkedGUID = GUID.GUID
type OriginalGUID = GUID.GUID
type PartialFingerprint = Fingerprint
type WatermarkSample = (OriginalGUID, WatermarkedGUID)

-- | Given the GUID the file is to be watermarked with and
--   a target GUID, watermark the GUID.
watermark :: ClientID -> GUID.GUID -> GUID.GUID
watermark (GUID.preprocess -> clientid) (GUID.preprocess -> guid) =
    let fingerprint = clientIDToFingerprint clientid
        patternNum  = fromIntegral $ hexToInteger guid `mod` toInteger numPatterns
        bitmask =
            take GUID.guidLength . drop (GUID.guidLength * patternNum) $ fingerprint
        guidUpper = map toUpper guid
    in GUID.reformat $ zipWith applyMask bitmask guidUpper
  where
    applyMask :: Char -> Char -> Char
    applyMask '0' c = c
    applyMask '1' c = incrementHex c
    applyMask _   c = c

-- | Increment a hexadecimal digit, wrapping F to 0.
incrementHex :: Char -> Char
incrementHex c = hexDigit ((hexCharValue c + 1) `mod` 16)

-- | Convert an integer (0-15) to its hexadecimal digit.
hexDigit :: Int -> Char
hexDigit n
    | n < 10    = intToDigit n
    | otherwise = toUpper (intToDigit n)

-- | Convert a hexadecimal digit character to its numeric value.
hexCharValue :: Char -> Int
hexCharValue ch = fromIntegral . hexToInteger $ [toUpper ch]

-- | Given a list of original and watermarked GUID pairs, recover the Client GUID
--   with which the bitmask used to watermark them was generated.
recoverClientID :: [WatermarkSample] -> ClientID
recoverClientID = fingerprintToClientID . recoverFingerprint

-- | Given a list of original and watermarked GUID pairs, recover the bitmask used to watermark them
recoverFingerprint :: [WatermarkSample] -> Fingerprint
recoverFingerprint samples = recoverFingerprint' normalizedSamples
  where
    normalizedSamples = map normalize samples
    normalize (GUID.preprocess -> original, GUID.preprocess -> watermarked) =
        (map toUpper original, map toUpper watermarked)
    recoverFingerprint' :: [WatermarkSample] -> Fingerprint
    recoverFingerprint' =
        concatMap (mergePartials . map recoverPartial) . groupSamples
    groupSamples :: [WatermarkSample] -> [[WatermarkSample]]
    groupSamples =
        L.groupBy sameGroup . L.sortOn (getGroup . fst)
    sameGroup (o1, _) (o2, _) = getGroup o1 == getGroup o2
    getGroup :: GUID.GUID -> Int
    getGroup = (`mod` numPatterns) . fromIntegral . hexToInteger

-- | Given a single original and watermarked GUID pair, recover as much
--   information about the fingerprint as possible.
recoverPartial :: WatermarkSample -> PartialFingerprint
recoverPartial ([], []) = []
recoverPartial (o : os, w : ws) =
    recoverBit o w : recoverPartial (os, ws)
recoverPartial _ = []

recoverBit :: Char -> Char -> Char
recoverBit original watermarked =
    let delta = (hexCharValue watermarked - hexCharValue original) `mod` 16
    in if delta == 1 then '1' else '0'

-- | Given all partial fingerprints found, reconstruct the full fingerprint
mergePartials :: [PartialFingerprint] -> Fingerprint
mergePartials ps = [ maximum (map (!! i) ps) | i <- [0 .. GUID.guidLength - 1] ]
