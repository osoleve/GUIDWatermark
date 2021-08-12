# Watermark
Proof of concept for discreetly watermarking a database extract for distribution to unsecured third parties, supporting up to 2^128 recipients.

For educational purposes only. If you want to use this in production, please contact me first so I can talk you out of it.

```haskell
import Watermark

-- recoverClientID takes a list of watermarked GUIDs
-- Sample GUIDs watermarked with ClientID A0562609-1AF7-DD39-A2B4-35E71B3A14BC
main = lines <$> readFile "test/watermarked_guids.txt"
```

```
> guids = main
> recoverClientID <$> guids
"A0562609-1AF7-DD39-A2B4-35E71B3A14BC"
```