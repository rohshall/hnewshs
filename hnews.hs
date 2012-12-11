{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Network.HTTP
import Network.URI
import Text.Regex.Posix
import Text.Regex.Posix.ByteString

import GHC.Generics (Generic)

data Article = Article { title :: BS.ByteString,
                         url :: BS.ByteString,
                         score :: BS.ByteString,
                         user :: BS.ByteString,
                         comments :: BS.ByteString,
                         time :: BS.ByteString,
                         item_id :: BS.ByteString,
                         description :: BS.ByteString
               } deriving (Show, Generic)

data Articles = Articles { items  :: [Maybe Article] }
                deriving (Show, Generic)

instance FromJSON Articles
instance FromJSON Article

first :: (a, a, a) -> a
first (x, _, _) = x

main = do
  feed <- getFeed
  putStrLn $ BS.unpack feed
  let lazyFeed = BL.fromStrict feed
  let articles = decode lazyFeed :: Maybe Articles
  case articles of
    Just as -> printArticles as
    Nothing -> error "No Articles"


getTestFeed :: IO BS.ByteString
getTestFeed = do
  let feedProcessed = BS.pack "{\"items\":[{\"title\":\"How a load-balancing bug led to worldwide Chrome crashes\",\"url\":\"http://code.google.com/p/chromium/issues/detail?id=165171#c27\",\"score\":\"77 points\",\"user\":\"jpdus\",\"comments\":\"17 comments\",\"time\":\"3 hours ago\",\"item_id\":\"4904208\",\"description\":\"77 points by jpdus 3 hours ago  | 17 comments\"},{\"title\":\"New board in the Arduino Family: Esplora\",\"url\":\"http://arduino.cc/en/Main/ArduinoBoardEsplora\",\"score\":\"191 points\",\"user\":\"iamwil\",\"comments\":\"45 comments\",\"time\":\"21 hours ago\",\"item_id\":\"4900442\",\"description\":\"191 points by iamwil 21 hours ago  | 45 comments\"}]}"
  return feedProcessed

getFeed :: IO BS.ByteString
getFeed = do
  feed <- openURL "http://hndroidapi.appspot.com/news/format/json/page/"
  let pattern = ",\\{[^\\}]*\\}\\]\\}$" :: BS.ByteString
  let matchedFeed = feed =~ pattern :: (BS.ByteString, BS.ByteString, BS.ByteString)
  let suffix = "]}" :: BS.ByteString
  let feedProcessed = BS.append (first matchedFeed) suffix
  return feedProcessed


printArticles :: Articles -> IO()
printArticles articles = mapM_ printArticle $ catMaybes $ items articles


printArticle :: Article -> IO()
printArticle article = do
  putStrLn $ (show $ item_id article) ++ ": " ++ (show $ title article)
  putStrLn $ show $ url article
  putStrLn $ "(" ++ (show $ comments article) ++ ") (" ++ (show $ score article) ++ ")"
  putStrLn ""


openURL :: String -> IO BS.ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

