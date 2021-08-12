import           Watermark

-- Sample GUIDs watermarked with ClientID 65484653464635436017066
main = recoverClientID . lines <$> readFile "test/watermarked_guids.txt"
