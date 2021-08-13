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

import           Data.Char
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord

import           Watermark.Utils.Conversion
import qualified Watermark.Utils.GUID          as GUID

type WatermarkedGUID = GUID.GUID
type PartialFingerprint = Fingerprint

-- | Given the GUID the file is to be watermarked with and
--   a target GUID, watermark the GUID.
watermark :: ClientID -> GUID.GUID -> GUID.GUID
watermark (GUID.preprocess -> clientid) (GUID.preprocess -> guid) =
    let
        processChar :: Char -> Char -> Char
        processChar b c | b == '0' || isDigit c = c
                        | otherwise             = toLower c
        fingerprint = clientIDToFingerprint clientid
        patternNum  = fromIntegral $ hexToInteger guid `mod` toInteger numPatterns
        bitmask =
            take GUID.guidLength . drop (GUID.guidLength * patternNum) $ fingerprint
    in
        GUID.reformat $ zipWith processChar bitmask guid

-- | Given a list of watermarked GUIDs, recover the Client GUID
--   with which the bitmask used to watermark them was generated.
recoverClientID :: [WatermarkedGUID] -> ClientID
recoverClientID = fingerprintToClientID . recoverFingerprint

-- | Given a list of watermarked GUIDs, recover the bitmask used to watermark them
recoverFingerprint :: [WatermarkedGUID] -> Fingerprint
recoverFingerprint (map GUID.preprocess -> guids) = recoverFingerprint' guids
  where
    getGroup :: GUID.GUID -> Int
    getGroup    = (`mod` numPatterns) . fromIntegral . hexToInteger
    groupGUIDs :: [GUID.GUID] -> [[GUID.GUID]]
    groupGUIDs  = L.groupBy (\x y -> getGroup x == getGroup y) . L.sortOn getGroup
    getPartials :: [[GUID.GUID]] -> [[PartialFingerprint]]
    getPartials = (map . map) recoverPartial
    recoverFingerprint' :: [GUID.GUID] -> Fingerprint
    recoverFingerprint' = concatMap mergePartials . getPartials . groupGUIDs

-- | Given a watermarked GUID, recover as much information about the
--   fingerprint as possible.
recoverPartial :: WatermarkedGUID -> PartialFingerprint
recoverPartial [] = []
recoverPartial (g : gs) | isLower g = '1' : recoverPartial gs
                        | otherwise = '0' : recoverPartial gs

-- | Given all partial fingerprints found, reconstruct the full fingerprint
mergePartials :: [PartialFingerprint] -> Fingerprint
mergePartials ps = [ maximum (map (!! i) ps) | i <- [0 .. GUID.guidLength - 1] ]
