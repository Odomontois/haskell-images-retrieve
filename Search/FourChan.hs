module Search.FourChan where

import Search

data FourChan = FourChan

instance Searchable FourChan where
    patterns        FourChan = ["<a class=\"fileThumb\">"]
    extractImgURL   FourChan = extractAttr "href"
instance TitlePage FourChan where
    titlePageURL    FourChan = "http://boards.4chan.org/b/"
    pagePatterns    FourChan = ["<a class=\"replylink\">"]
    extractPageURL  FourChan = extractAttr "href"


