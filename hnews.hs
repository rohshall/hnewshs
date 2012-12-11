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
                         id :: Integer,
                         commentCount :: Integer,
                         points :: Integer,
                         postedAgo :: BS.ByteString,
                         postedBy :: BS.ByteString
               } deriving (Show, Generic)

data Articles = Articles { nextId :: Maybe Integer,
                           items  :: [Article],
                           version :: BS.ByteString,
                           cachedOnUTC :: BS.ByteString
                } deriving (Show, Generic)

instance FromJSON Articles
instance FromJSON Article

main = do
  feed <- openURL "http://api.ihackernews.com/page"
  let lazyFeed = BL.fromStrict feed
  let parseResult = eitherDecode lazyFeed :: Either String Articles
  case parseResult of
    Right articles -> printArticles articles
    Left s -> error ("Server error: " ++ s)


printArticles :: Articles -> IO()
printArticles articles = mapM_ printArticle $ items articles


printArticle :: Article -> IO()
printArticle article = do
  putStrLn $ (show $ Main.id article) ++ ": " ++ (show $ title article)
  putStrLn $ show $ url article
  putStrLn $ "(" ++ (show $ commentCount article) ++ ") (" ++ (show $ points article) ++ ")"
  putStrLn ""


openURL :: String -> IO BS.ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

