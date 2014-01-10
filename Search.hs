{-# LANGUAGE RankNTypes #-}
module Search where

import Text.HTML.TagSoup
import Text.StringLike
import Control.Applicative
import Data.List
import Data.Maybe

type ExtractURL str = StringLike str=>[Tag str]->Maybe str
type Search str = StringLike str=>[Tag str]->[[Tag str]]

extractAttr::StringLike str=>String->ExtractURL str
extractAttr attrName (TagOpen tag attributes:tags) = snd <$> find source attributes where
    source attribute    = fst  attribute == ( fromString attrName )
extractAttr attrName otherTags = Nothing

patternSearch::StringLike str=>[String]->[Tag str]->[[Tag str]]
patternSearch patterns = let
        iter::StringLike str=>[[Tag str]]->[String]->[[Tag str]]
        iter taglist [] = taglist
        iter taglist (p:ps) = do    
            tags <- taglist
            iter ( sections (~== p) tags) ps
        search tags = iter [tags] $ patterns
        in search

extractURLs::StringLike str=>Search str->ExtractURL str->str->[str]
extractURLs search extract source = catMaybes $ map extract $ search $ parseTags source 
    
class Searchable site where
    patterns::site->[String]
    extractImgURL::StringLike str=>site->ExtractURL str

    search::StringLike str=>site->Search str
    search = patternSearch . patterns

    extractImageURLs::StringLike str=>site->str->[str]
    extractImageURLs site =  extractURLs ( search site ) ( extractImgURL site )

class Searchable site=>TitlePage site where
    titlePageURL::site->String
    pagePatterns::site->[String]
    extractPageURL::StringLike str=>site->ExtractURL str

    searchPage::StringLike str=>site->Search str
    searchPage = patternSearch . pagePatterns

    extractPageURLs::StringLike str=>site->str->[str]
    extractPageURLs site = extractURLs (searchPage site) (extractPageURL site) 





