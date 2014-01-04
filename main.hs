import Utils.IO
import Text.HTML.TagSoup
import Search
import Search.FourChan
import Search.Network
import Network.URI
import Control.Applicative
import qualified Data.ByteString as B
import Data.String.Utils
import System.Environment

main = do
    [category, url] <- getArgs   
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