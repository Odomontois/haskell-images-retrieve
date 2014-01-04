module Utils.IO where
import System.IO 
import Network.HTTP
import Control.Applicative
import qualified Data.ByteString as B
import Network.URI
import System.Directory
import Data.String.Utils

writeUTF8File:: String -> String -> IO()
writeUTF8File = writeFile where
    writeFile fileName src = withFile fileName WriteMode (write src)
    write src file = do
        hSetEncoding file utf8
        hPutStr file src

getBinContent:: String -> IO B.ByteString
getBinContent = getBin where
    getBin url = do
        case  parseURI url of
            Nothing -> return B.empty  
            Just uri -> getResponseBody =<< simpleHTTP (mkRequest GET uri)


ensureDirectory:: String -> IO ()
ensureDirectory name = do
    exists <- doesDirectoryExist name
    case reverse $ split "/" name of
         end:revPrefix -> ensureDirectory $ join "/" $ reverse revPrefix
         singleDir     -> return ()
    if not exists 
        then do 
            putStrLn $ "creating directory " ++ name
            createDirectory name 
        else return ()

skipExistingFile:: String -> IO () -> IO ()
skipExistingFile name action = do
    exists <- doesFileExist name
    if not exists
        then action
        else putStrLn $ "skipping " ++ name ++ " already exists"




