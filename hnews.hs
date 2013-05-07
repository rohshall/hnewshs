import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.HTTP.Wget
import Text.XML.Light


showArticle :: Element -> Maybe String
showArticle elem = do
    titleElem <- findChild (unqual "title") elem
    linkElem <- findChild (unqual "link") elem
    return $ concat $ map (\c -> strContent c ++ "\n") [titleElem, linkElem]


main = do
    feed <- wget "https://news.ycombinator.com/rss" [] []
    case parseXMLDoc feed of
        Nothing -> error "Failed to parse the feed"
        Just doc -> 
            let articles = findElements (unqual "item") $ doc
            in mapM_ (putStrLn.fromJust.showArticle) articles

