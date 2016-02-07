module Main where
import qualified Data.Text as T
import Network.TextServer

counter _ i = (T.pack $ "msgs: " ++ (show i), i+1)

main = runServer 0 counter
