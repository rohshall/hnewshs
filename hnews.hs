import Data.Aeson
import Data.Attoparsec
import Data.ByteString
import Data.Maybe
import Network.HTTP
import Network.URI


main = do
  feed <- openURL "http://hndroidapi.appspot.com/news/format/json/page/"
  print $ parse json feed

openURL :: String -> IO ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))
