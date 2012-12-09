{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
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

data Articles = Articles {nextId :: Maybe Integer, 
                          items  :: [Article],
                          version :: BS.ByteString,
                          cachedOnUTC :: BS.ByteString}
                deriving (Show, Generic)

instance FromJSON Articles
instance FromJSON Article

main = do
  feed <- openURL "http://api.ihackernews.com/page"
  let lazyFeed = BL.fromStrict feed
  let articles = decode lazyFeed :: Maybe Articles
  case articles of
    Just as -> printArticles as
    Nothing -> error "No Articles"


printArticles :: Articles -> IO()
printArticles articles = mapM_ printArticle $ items articles


printArticle :: Article -> IO()
printArticle article = do
  putStrLn $ (show $ Main.id article) ++ ": " ++ (show $ title article)
  putStrLn $ show $ url article
  putStrLn $ "(" ++ (show $ commentCount article) ++ " comments) (" ++ (show $ points article) ++ " points)"
  putStrLn ""


openURL :: String -> IO BS.ByteString
openURL url = getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

