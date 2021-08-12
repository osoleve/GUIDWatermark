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

type GUID = String
type ClientID = Integer -- ^ Int in range [0, 2^32-1]
type WatermarkedGUID = String
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
clientIDToFingerprint = toFullLength . concatMap show . tail . toBin
  where
    fullLength = guidLength * fromIntegral numPatterns
    toFullLength :: String -> String
    toFullLength b | length b <= fullLength = leftPad (fullLength - length b) b
                   | otherwise              = take fullLength b

leftPad :: Int -> String -> String
leftPad 0 s = s
leftPad n s = replicate n '0' ++ s

toBin :: Integer -> [Int]
toBin 0 = [0]
toBin n = flip (++) [n' `mod` 2] $ toBin (n `div` 2) where n' = fromIntegral n

hexToInteger :: String -> Integer
hexToInteger [] = 0
hexToInteger h  = hexCharToInt (last h) + (16 * hexToInteger (init h))
  where
    hexCharToInt :: Char -> Integer
    hexCharToInt (toLower -> c) =
        toInteger . fromJust $ c `L.elemIndex` "0123456789abcdef"

integerToGUID' :: Integer -> GUID
integerToGUID' 0 = []
integerToGUID' n = intToHexChar (n `mod` 16) : integerToGUID' (n `div` 16)
  where
    intToHexChar :: Integer -> Char
    intToHexChar x | x < 10    = head $ show x
                   | otherwise = chr (fromIntegral $ 65 + (x - 10))

integerToGUID :: Integer -> GUID
integerToGUID 0 = replicate 32 '0'
integerToGUID x = leftPad n x'
  where
    x' = integerToGUID' x
    n = 32 - length x'

fingerprintToClientID :: Fingerprint -> ClientID
fingerprintToClientID = toDec
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
