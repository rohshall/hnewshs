{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.HTTP
import Network.URI

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

main = do
  feed <- openURL "http://hndroidapi.appspot.com/news/format/json/page/"
  let lazyFeed = BL.fromStrict feed
  let parseResult = eitherDecode lazyFeed :: Either String Articles
  case parseResult of
    Right articles -> printArticles articles
    Left s -> error ("Server error: " ++ s)


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

