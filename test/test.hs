import           Watermark

-- Sample GUIDs watermarked with ClientID A0562609-1AF7-DD39-A2B4-35E71B3A14BC
main = recoverClientID . lines <$> readFile "test/watermarked_guids.txt"
