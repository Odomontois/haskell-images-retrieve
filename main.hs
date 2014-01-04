import Utils.IO
import Text.HTML.TagSoup
import Search
import Search.FourChan
import Search.Network
import Network.URI
import Control.Applicative

main = let url = "//asd/sdf/sdfsdfasdf.jpg" 
    in do 
    images <- extractImages url
    print $ show images