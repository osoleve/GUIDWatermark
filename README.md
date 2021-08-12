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

> clientIDGUID = "A0562609-1AF7-DD39-A2B4-35E71B3A14BC"
clientIDGUID :: [Char]

> map (watermark clientIDGUID) guids
["26cCDA16-1077-Fb69-720B-2154ABB48E98"
,"9FF085ec-DA5d-2EcF-532f-AeEA0974aeEB"
,"e6a93E8C-580a-6aAB-A46f-28566544F8ba"
,"F841b897-9454-60a5-8C81-09AF3F244f1B"
,"91F5aFd7-8B18-23d0-E0B7-B6D24D54ff3B"
,"D449eDaE-665d-8abb-c44d-710a4A046E99"
,"A9695C53-7Da6-3Ea0-3CB8-01831De4df6B"
,"16187Ea0-F097-6950-385c-Ca1B81a4edEB"
,"e09D2014-88d4-18D4-FB95-28B81c64F58a"
,"D66c25c8-4f7b-6d12-daCe-acCa08843379"]
it :: [Watermark.GUID]
```

```haskell
> import Watermark

> guids = lines <$> readFile "test/watermarked_guids.txt"
guids :: IO [String]

-- recoverClientID takes a list of watermarked GUIDs
-- Sample GUIDs watermarked with ClientID A0562609-1AF7-DD39-A2B4-35E71B3A14BC
> recoverClientID <$> guids
"A0562609-1AF7-DD39-A2B4-35E71B3A14BC"
it :: Watermark.ClientID
```