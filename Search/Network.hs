module Search.Network where

import Search
import Network.HTTP
import Network.URI
import Text.StringLike
import Data.Maybe

data Image = Image{link::String, name::String} deriving (Show, Eq, Ord, Read)

extractImages:: (Searchable site)=>site->String->IO[Image]
extractImages site url = do
    source <- getResponseBody =<< simpleHTTP (getRequest url)  
    let siteUri = parseURI url
        siteAuth = siteUri >>= uriAuthority
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

extractPages::(TitlePage site)=>site->IO[String]
extractPages site = let 
    title = titlePageURL site
    in do
        source <- getResponseBody =<< simpleHTTP (getRequest title)
        return $ map (title ++) $ extractPageURLs site source

extractAllImages::(TitlePage site)=>site->IO[(String,[Image])]
extractAllImages site = do
    pages <- extractPages site
    let extract page = do
        images <- extractImages site page
        return(page,images)
    sequence $ map extract pages