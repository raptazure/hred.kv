{-# LANGUAGE OverloadedStrings #-}

module Lib
  (
  )
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, readTVar)
import Data.Attoparsec.ByteString (count)
import Data.Attoparsec.ByteString.Char8
  ( Parser,
    char,
    choice,
    decimal,
    endOfLine,
    signed,
    take,
  )
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Network.Socket
import Prelude hiding (lookup, take)

type Value = ByteString

type Key = ByteString

type DB = Map Key Value

data Command
  = Get Key
  | Set Key Value
  | Unkown
  deriving (Eq, Show)

data Reply
  = Bulk (Maybe ByteString)
  | MultiBulk (Maybe [Reply])
  deriving (Eq, Show)

parseReply :: Reply -> Maybe Command
parseReply (MultiBulk (Just xs)) =
  case xs of
    [Bulk (Just "get"), Bulk (Just a)] -> Just $ Get a
    [Bulk (Just "set"), Bulk (Just a), Bulk (Just b)] -> Just $ Set a b
    _ -> Just Unkown
parseReply _ = Nothing

replyParser :: Parser Reply
replyParser = choice [bulk, multiBulk]

bulk :: Parser Reply
bulk =
  Bulk <$> do
    len <- char '$' *> signed decimal <* endOfLine
    if len < 0
      then return Nothing
      else Just <$> take len <* endOfLine

multiBulk :: Parser Reply
multiBulk =
  MultiBulk <$> do
    len <- char '*' *> signed decimal <* endOfLine
    if len < 0
      then return Nothing
      else Just <$> count len replyParsert

sockHandler :: Socket -> TVar DB -> IO ()
sockHandler sock db = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  hSetBinaryMode handle True
  _ <- forkIO $ commandProcessor handle db
  sockHandler sock db

-- STM (software transactional memory) - change the value

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn