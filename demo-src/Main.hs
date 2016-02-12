module Main where
import qualified Data.Text.Lazy as T (pack)
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8)
import Network.TextServer

counter _ i = return (Just $ T.encodeUtf8 $ T.pack $ "msgs: " ++ (show i), i+1)

main = runServer 0 counter
