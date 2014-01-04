module Search where
import Text.HTML.TagSoup
import Text.StringLike
import Control.Applicative
import Data.Maybe
    
class Searchable site where
    patterns:: site -> [String]

    searchPatterns::StringLike str => site -> [str]
    searchPatterns site= fromString <$> patterns site

    extractImgURL::StringLike str => site -> [Tag str] -> Maybe str

    search ::StringLike str => site -> [Tag str] -> [[Tag str]]
    search = let
        iter :: StringLike str => [[Tag str]] -> [String] -> [[Tag str]]
        iter taglist [] = taglist
        iter taglist (pattern:patterns) = do
            tags <- taglist
            iter ( sections (~== pattern) tags) patterns
        searchInt site tags =  iter [tags] $ searchPatterns site
        in searchInt

    extractImageURLs:: StringLike str => site -> str -> [str]
    extractImageURLs site source = catMaybes $ map (extractImgURL site) $ search site $ parseTags source

