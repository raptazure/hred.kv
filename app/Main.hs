module Main where

import Control.Concurrent.STM
import Data.Map
import Lib
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  database <- atomically $ newTVar $ fromList [("__version__", "0.1.0")]
  sock <- listen 7777
  sockHandler sock database