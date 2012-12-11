{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.HTTP
import Network.URI

import GHC.Generics (Generic)

data Article = Article { rank :: BS.ByteString,
                         title :: BS.ByteString,
                         url :: BS.ByteString
               } deriving (Show, Generic)

instance FromJSON Article

main = do
  feed <- openURL "http://apify.heroku.com/api/hacker_news.json"
  let lazyFeed = BL.fromStrict feed
  let parseResult = eitherDecode lazyFeed :: Either String [Article]
  case parseResult of
    Right articles -> mapM_ printArticle articles
    Left s -> error ("Server error: " ++ s)


printArticle :: Article -> IO()
printArticle article = do
  putStrLn $ (show $ rank article) ++ ": " ++ (show $ title article)
  putStrLn $ show $ url article
  putStrLn ""


openURL :: String -> IO BS.ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

