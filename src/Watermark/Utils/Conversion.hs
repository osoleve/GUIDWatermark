{-# LANGUAGE ViewPatterns #-}

module Watermark.Utils.Conversion
    (leftPad, toBin, hexToInteger, integerToGUID) where

import           Data.Char
import qualified Data.List                     as L
import           Data.Maybe
import           Data.Ord

type GUID = String

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
    n  = 32 - length x'
