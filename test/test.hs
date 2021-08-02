import           Watermark

-- Sample GUIDs watermarked with ClientID 1671437891
main = recoverClientID . lines <$> readFile "test/watermarked_guids.txt"
