module Search.FourChan where

import Search
import Text.HTML.TagSoup
import Control.Applicative
import Data.List
import Text.StringLike

data FourChan = FourChan

instance Searchable FourChan where
    patterns FourChan = ["<a class=\"fileThumb\">"]
    extractImgURL FourChan (TagOpen tag attributes:tags) = snd <$> find source attributes where
        source attribute = fst  attribute == href
        href = fromString "href"
    extractImgURL FourChan othertags = Nothing
