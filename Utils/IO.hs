module Utils.IO where
import System.IO 
import Network.HTTP

writeUTF8File = writeFile where
    writeFile fileName src = withFile fileName WriteMode (write src)
    write src file = do
        hSetEncoding file utf8
        hPutStr file src


