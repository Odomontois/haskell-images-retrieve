module Search.Network where

import Search
import Network.HTTP
import Network.URI
import Text.StringLike
import Data.Maybe

data Image = Image{link::String, name::String} deriving (Show, Eq, Ord, Read)

extractImages:: (Searchable site) => site -> String -> IO[Image]
extractImages site url = let
    siteUri = parseURI url
    siteAuth = do 
        uri <- siteUri
        uriAuthority uri
    siteScheme = do
        uri <- siteUri
        uriScheme uri
    parseImage imageURL = do
        scheme <- siteScheme
        imageUri <- parseURI imageURL
        let path = uriPath imageUri
            name = path
            link = URI scheme siteAuth path "" ""
        Image name link
    in do
    source <- getResponseBody =<< simpleHTTP (getRequest url)    
    return $ catMaybes $ map parseImage $ extractImageURLs site source
