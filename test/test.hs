import           Control.Monad                 (unless)
import           System.Exit                   (exitFailure)

import           Watermark
import qualified Watermark.Utils.Conversion    as Conversion
import qualified Watermark.Utils.GUID          as GUID

clientGUID :: ClientID
clientGUID = "A0562609-1AF7-DD39-A2B4-35E71B3A14BC"

totalFingerprintLength :: Int
totalFingerprintLength = GUID.guidLength * Conversion.numPatterns

chunkSize :: Int
chunkSize = GUID.guidLength

patternFingerprint :: Conversion.Fingerprint
patternFingerprint =
    concatMap segment [0 .. Conversion.numPatterns - 1]
  where
    segment idx
        | even idx  = replicate chunkSize '1'
        | otherwise = replicate chunkSize '0'

patternClientGUID :: ClientID
patternClientGUID = fingerprintToClientID patternFingerprint

mkOriginal :: Integer -> GUID.GUID
mkOriginal = GUID.reformat . Conversion.integerToGUID

incrementHexChar :: Char -> Char
incrementHexChar c = digits !! nextIndex
  where
    digits = "0123456789ABCDEF"
    value = fromIntegral (Conversion.hexToInteger [c])
    nextIndex = (value + 1) `mod` length digits

main :: IO ()
main = do
    runTest "encode/decode roundtrip preserves client IDs" testRoundTrip
    runTest "watermark leaves GUID unchanged when mask is zero" testWatermarkZeroFingerprint
    runTest "watermark increments digits when mask bits are set" testWatermarkAllOnesFingerprint
    runTest "watermark applies pattern-specific slices" testWatermarkPatternSelection
    runTest "recoverFingerprint reconstructs full mask from synthetic samples" testRecoverFingerprintRoundTrip
    runTest "fingerprint structure matches expected format" testFingerprintStructure
    runTest "decoder handles extreme fingerprints" testDecoderExtremes
    runTest "recoverClientID recovers sample client ID" testRecoverClientID
    putStrLn "[PASS] All encode/decode tests passed"

runTest :: String -> IO () -> IO ()
runTest label action = do
    putStrLn ("[RUN ] " ++ label)
    action
    putStrLn ("[ OK ] " ++ label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (actual == expected) $
        failTest label ("expected " ++ show expected ++ ", but got " ++ show actual)

assertBool :: String -> Bool -> IO ()
assertBool label condition =
    unless condition $
        failTest label "condition was False"

failTest :: String -> String -> IO ()
failTest label message = do
    putStrLn ("[FAIL] " ++ label)
    putStrLn ("       " ++ message)
    exitFailure

testRoundTrip :: IO ()
testRoundTrip = mapM_ check sampleClientIDs
  where
    sampleClientIDs =
        [ clientGUID
        , "00000000-0000-0000-0000-000000000000"
        , "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"
        , "12345678-90AB-CDEF-0123-456789ABCDEF"
        ]
    check raw = do
        let normalized = GUID.preprocess raw
            fingerprint = clientIDToFingerprint normalized
            recovered = fingerprintToClientID fingerprint
        assertEqual ("clientID roundtrip for " ++ raw) raw recovered

testWatermarkZeroFingerprint :: IO ()
testWatermarkZeroFingerprint = do
    let zeroClient = GUID.reformat (replicate GUID.guidLength '0')
        original = GUID.reformat (replicate GUID.guidLength 'A')
        watermarked = watermark zeroClient original
    assertEqual "watermark zero fingerprint" original watermarked

testWatermarkAllOnesFingerprint :: IO ()
testWatermarkAllOnesFingerprint = do
    let onesClient = GUID.reformat (replicate GUID.guidLength 'F')
        original = GUID.reformat (replicate GUID.guidLength '0')
        expected = GUID.reformat (replicate GUID.guidLength '1')
        watermarked = watermark onesClient original
    assertEqual "watermark all-ones fingerprint" expected watermarked

testWatermarkPatternSelection :: IO ()
testWatermarkPatternSelection =
    mapM_ checkPattern [0 .. Conversion.numPatterns - 1]
  where
    checkPattern idx = do
        let original = mkOriginal (fromIntegral idx)
            watermarked = watermark patternClientGUID original
            originalNormalized = GUID.preprocess original
            watermarkedNormalized = GUID.preprocess watermarked
            chunkStart = idx * chunkSize
            chunkMask = take chunkSize . drop chunkStart $ patternFingerprint
            expectedNormalized = zipWith applyMask chunkMask originalNormalized
        assertEqual ("pattern-specific mask for chunk " ++ show idx) expectedNormalized watermarkedNormalized
    applyMask '1' c = incrementHexChar c
    applyMask _   c = c

testRecoverFingerprintRoundTrip :: IO ()
testRecoverFingerprintRoundTrip = do
    let pairs = map makePair [0 .. Conversion.numPatterns - 1]
        makePair idx =
            let original = mkOriginal (fromIntegral idx)
                watermarked = watermark patternClientGUID original
            in (original, watermarked)
        recovered = recoverFingerprint pairs
    assertEqual "recoverFingerprint synthetic samples" patternFingerprint recovered
    assertEqual "recoverClientID synthetic samples" patternClientGUID (recoverClientID pairs)

testFingerprintStructure :: IO ()
testFingerprintStructure = do
    let normalized = GUID.preprocess clientGUID
        fingerprint = clientIDToFingerprint normalized
    assertEqual "fingerprint length" totalFingerprintLength (length fingerprint)
    assertBool "fingerprint contains only binary digits" (all (`elem` ("01" :: String)) fingerprint)
    let zeroClient = replicate GUID.guidLength '0'
    assertEqual "zero client fingerprint" (replicate totalFingerprintLength '0') (clientIDToFingerprint zeroClient)

testDecoderExtremes :: IO ()
testDecoderExtremes = do
    let zeroFingerprint = replicate totalFingerprintLength '0'
        maxFingerprint = replicate totalFingerprintLength '1'
        expectedZero = GUID.reformat (replicate GUID.guidLength '0')
        expectedMax = GUID.reformat (replicate GUID.guidLength 'F')
    assertEqual "decode zero fingerprint" expectedZero (fingerprintToClientID zeroFingerprint)
    assertEqual "decode max fingerprint" expectedMax (fingerprintToClientID maxFingerprint)

testRecoverClientID :: IO ()
testRecoverClientID = do
    originals <- map GUID.reformat . lines <$> readFile "test/guids.txt"
    watermarked <- lines <$> readFile "test/watermarked_guids.txt"
    assertEqual "sample data has matching lengths" (length originals) (length watermarked)
    let pairs = zip originals watermarked
    assertEqual "recoverClientID from sample data" clientGUID (recoverClientID pairs)
