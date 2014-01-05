import Utils.IO
import Text.HTML.TagSoup
import Search
import Search.FourChan
import Search.Network
import Network.URI
import Network.HTTP
import Control.Applicative
import qualified Data.ByteString as B
import Data.String.Utils
import System.Environment
import Data.Maybe
import Data.List as L

readArgs args = case args of 
    args@(cat:url:rest) -> return [cat,url]
    [x]                 -> return ["default",x]
    []                  -> do
        source <- getResponseBody =<< simpleHTTP (getRequest "http://boards.4chan.org/b/")
        let reply tag               = tag ~== "<a class=\"replylink\">"
            tags                    = parseTags source
            replies                 = sections reply tags
            first                   = head $ head $ replies
            url (TagOpen _ attrs)   = "http://boards.4chan.org/b/" ++ (snd $ fromJust $ find href attrs)
            url tag                 = ""
            href attr               = fst attr == "href"
        return ["default", url first]
main = do
    [category, url] <- getArgs >>= readArgs 
    images          <- extractImages FourChan url
    content         <- getBinContent $ link $ images !! 0
    let thread           = last $ split "/" url
        directory        = category ++ "/" ++ thread  
        writeImage image = do
            let fileName = directory ++ "/" ++(last $ split "/" $ name image)
                writeFile = do
                    content <- getBinContent $ link image       
                    putStrLn ( "writing new " ++ fileName ) 
                    B.writeFile fileName content             
            skipExistingFile fileName writeFile
    ensureDirectory directory
    sequence $ map writeImage images 