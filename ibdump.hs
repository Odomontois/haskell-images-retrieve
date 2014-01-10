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

site = FourChan

readArgs args = return $ case args of 
    args@(cat:url:rest) -> (cat, Just url)
    [x]                 -> ("default", Just x)
    []                  -> ("default", Nothing)

getImages (category,(Just url)) = do
    images <- extractImages site url
    return [(url, images)]

getImages (category,Nothing) = extractAllImages site

writeImages category (url,images) = 
    let thread           = last $ split "/" url
        directory        = category ++ "/" ++ thread  
        writeImage image = 
            let fileName = directory ++ "/" ++(last $ split "/" $ name image)
                writeFile = do
                    content <- getBinContent $ link image       
                    putStrLn ( "writing new " ++ fileName ) 
                    B.writeFile fileName content             
            in do skipExistingFile fileName writeFile
    in do   ensureDirectory directory
            sequence $ map writeImage images 


main = do
    args <- readArgs =<< getArgs
    images <- getImages args
    sequence $ map ( writeImages $ fst args ) images  