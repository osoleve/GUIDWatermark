{-# LANGUAGE ViewPatterns #-}

module Watermark.Utils.Conversion
    ( leftPad
    , toBin
    , hexToInteger
    , integerToGUID
    , clientIDToFingerprint
    , fingerprintToClientID
    , ClientID
    , Fingerprint
    , numPatterns
    ) where

import           Data.Char
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord

import qualified Watermark.Utils.GUID as GUID

type ClientID = GUID.GUID
type Fingerprint = String

numPatterns :: Int
numPatterns = 4

leftPad :: Int -> String -> String
leftPad 0 s = s
leftPad n s = replicate n '0' ++ s

toBin :: Integer -> [Int]
toBin 0 = []
toBin n = flip (++) [fromIntegral n `mod` 2] $ toBin (n `div` 2)

hexToInteger :: String -> Integer
hexToInteger [] = 0
hexToInteger h  = hexCharToInt (last h) + (16 * hexToInteger (init h))
  where
    hexCharToInt :: Char -> Integer
    hexCharToInt (toLower -> c) =
        toInteger . fromJust $ c `L.elemIndex` "0123456789abcdef"

integerToGUID' :: Integer -> GUID.GUID
integerToGUID' 0 = []
integerToGUID' n = intToHexChar (n `mod` 16) : integerToGUID' (n `div` 16)
  where
    intToHexChar :: Integer -> Char
    intToHexChar x | x < 10    = head $ show x
                   | otherwise = chr (fromIntegral $ 65 + (x - 10))

integerToGUID :: Integer -> GUID.GUID
integerToGUID 0 = replicate GUID.guidLength '0'
integerToGUID x = leftPad n x'
  where
    x' = integerToGUID' x
    n  = GUID.guidLength - length x'

clientIDToFingerprint :: ClientID -> Fingerprint
clientIDToFingerprint = toFullLength . concatMap show . toBin . hexToInteger
  where
    fullLength = GUID.guidLength * numPatterns
    toFullLength :: String -> String
    toFullLength b | length b <= fullLength = leftPad (fullLength - length b) b
                   | otherwise              = take fullLength b

fingerprintToClientID :: Fingerprint -> ClientID
fingerprintToClientID = GUID.reformat . integerToGUID . toDec
  where
    toDec :: String -> Integer
    toDec = L.foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0
