module Search.Network where

import Search
import Network.HTTP
import Network.URI
import Text.StringLike
import Data.Maybe

data Image = Image{link::String, name::String} deriving (Show, Eq, Ord, Read)

extractImages:: (Searchable site) => site -> String -> IO[Image]
extractImages site url = do
    source <- getResponseBody =<< simpleHTTP (getRequest url)  
    let siteUri = parseURI url
        siteAuth = do 
            uri <- siteUri
            uriAuthority uri
        siteScheme = do
            uri <- siteUri
            return $ uriScheme uri
        imageLink imageURL = do
            scheme <- siteScheme
            return $ scheme ++ imageURL
        parseImage imageURL = do
            link <- imageLink imageURL
            uri  <- parseURI link
            let name = uriPath uri            
            return $ Image link name
            
    return $ catMaybes $ map parseImage $ extractImageURLs site source