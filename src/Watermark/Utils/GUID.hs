module Watermark.Utils.GUID
    ( preprocess
    , reformat
    ) where

import qualified Data.List                     as L

type GUID = String

-- | Remove the hyphens from a GUID
preprocess :: GUID -> GUID
preprocess = filter (/= '-')

-- | Add the hyphens back into a GUID
reformat :: GUID -> GUID
reformat guid = L.intercalate "-" spans
  where
    spans = map
        ($ guid)
        [take 8, take 4 . drop 8, take 4 . drop 12, take 4 . drop 16, take 12 . drop 20]
