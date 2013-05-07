import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Network.HTTP.Wget
import Text.XML.Light

main = do
  feed <- wget "https://news.ycombinator.com/rss" [] []
  case parseXMLDoc feed of
    Nothing -> error "Failed to parse the feed"
    Just doc -> 
      let _:_:elem:_ = findElements (unqual "description") $ doc
          [Text title_content] = elContent elem
          title_text = cdData title_content
      in mapM_ putStrLn $ lines title_text

