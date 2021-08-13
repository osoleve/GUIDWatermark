{-# LANGUAGE ViewPatterns #-}

module Watermark.Utils.Conversion
    ( leftPad
    , toBin
    , hexToInteger
    , integerToGUID
    , clientIDToFingerprint
    , fingerprintToClientID
    ) where

import           Data.Char
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord

import Watermark.Utils.GUID

type GUID = String
type ClientID = GUID
type Fingerprint = String

guidLength = 32
numPatterns = 4

guidLength = 32

leftPad :: Int -> String -> String
leftPad 0 s = s
leftPad n s = replicate n '0' ++ s

toBin :: Integer -> [Int]
toBin 0 = [0]
toBin n = flip (++) [fromIntegral n `mod` 2] $ toBin (n `div` 2)

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
    n  = guidLength - length x'

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
