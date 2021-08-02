import Watermark

main = recoverFingerprint . lines <$> readFile "watermarked_guids.txt"