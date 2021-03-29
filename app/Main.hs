{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM (newTVarIO)
import Data.Map (fromList)
import Lib (sockHandler, version)
import Network.Socket
  ( AddrInfo
      ( addrAddress,
        addrFamily,
        addrFlags,
        addrProtocol,
        addrSocketType
      ),
    AddrInfoFlag (AI_NUMERICHOST, AI_NUMERICSERV),
    SocketType (Stream),
    bind,
    defaultHints,
    getAddrInfo,
    listen,
    socket,
    withSocketsDo,
  )

main :: IO ()
main = withSocketsDo $ do
  database <- newTVarIO (fromList [("__version__", version)])
  let hints = defaultHints {addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
  addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "7777")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 7777
  putStrLn "Listening on localhost 7777"
  sockHandler sock database
