# Watermark
Proof of concept for discreetly watermarking a database extract for distribution to unsecured third parties, supporting up to 2^128 recipients. Requires on average >20,000 GUIDs to work properly, but a weaker version not published here can work with fewer.

For educational purposes only. If you want to use this in production, please contact me first so I can talk you out of it.

```haskell
> import Watermark

> guids = ["26CCDA16-1077-FB69-720B-2154ABB48E98"
          ,"9FF085EC-DA5D-2ECF-532F-AEEA0974AEEB"
          ,"E6A93E8C-580A-6AAB-A46F-28566544F8BA"
          ,"F841B897-9454-60A5-8C81-09AF3F244F1B"
          ,"91F5AFD7-8B18-23D0-E0B7-B6D24D54FF3B"
          ,"D449EDAE-665D-8ABB-C44D-710A4A046E99"
          ,"A9695C53-7DA6-3EA0-3CB8-01831DE4DF6B"
          ,"16187EA0-F097-6950-385C-CA1B81A4EDEB"
          ,"E09D2014-88D4-18D4-FB95-28B81C64F58A"
          ,"D66C25C8-4F7B-6D12-DACE-ACCA08843379"]
guids :: [[Char]]

> clientGUID = "A0562609-1AF7-DD39-A2B4-35E71B3A14BC"
clientGUID :: [Char]

> map (watermark clientGUID) guids
["36DCDA16-1178-FC79-721B-2264ABB49E99"
,"9FF195FD-DA6E-3EDF-5320-AFEA1985BFEB"
,"F6B93E9C-681B-6BAB-A470-29577654F9CB"
,"F842C8A8-9465-70B5-8C82-0AAF4F35501B"
,"91F6BFE8-8B29-33E0-E0B8-B7D25D65003B"
,"D44AFDBE-776E-8BCC-D54E-820B4A157E9A"
,"A96A6C64-7DB7-4EB0-3CB9-02832DF5E06B"
,"16198EB1-F0A8-7960-385D-CB1B91B5FEEB"
,"F0AD2024-98E5-19D4-FBA6-29B92D74F69B"
,"D66D35D8-508C-6E23-EBCF-BDCB0895437A"]
it :: [Watermark.GUID]
```

```haskell
> import Watermark
> import qualified Watermark.Utils.GUID as GUID

> originals <- map GUID.reformat . lines <$> readFile "test/guids.txt"
originals :: [Watermark.GUID]

> watermarked <- lines <$> readFile "test/watermarked_guids.txt"
watermarked :: [Watermark.GUID]

-- recoverClientID now takes pairs of (original, watermarked) GUIDs
-- Sample GUIDs watermarked with ClientID A0562609-1AF7-DD39-A2B4-35E71B3A14BC
> recoverClientID (zip originals watermarked)
"A0562609-1AF7-DD39-A2B4-35E71B3A14BC"
it :: Watermark.ClientID
```
