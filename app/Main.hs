{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Data.Map
import Lib
import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  database <- atomically $ newTVar $ fromList [("__version__", version)]
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7777")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  Network.Socket.bind sock (addrAddress addr)
  putStrLn "Listening on localhost 7777"
  sockHandler sock database
