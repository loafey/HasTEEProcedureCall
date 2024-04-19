{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Binny (Bin (bin))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import Network.Run.TCP (runTCPClient, runTCPServer)
import Network.Socket.ByteString (recv, sendAll)

newtype Env = Env {counter :: Int}

updateRef :: (a -> a) -> IORef a -> IO ()
updateRef f r = readIORef r >>= writeIORef r . f
readRef :: IORef a -> IO a
readRef = readIORef

updateCounter :: IORef Env -> IO ()
updateCounter = updateRef $ \env -> env{counter = counter env + 1}
getCounter :: IORef Env -> IO Int
getCounter = (counter <$>) . readIORef

data Message
  = UpdateCounter
  | GetCounter
  deriving (Show, Generic, Bin)

-- toBytes :: Message -> BS.ByteString
-- toBytes = \case
--   UpdateCounter -> BS.singleton 0
--   GetCounter -> BS.singleton 1
fromBytes :: BS.ByteString -> Message
fromBytes b = case head . BS.unpack $ b of
  0 -> UpdateCounter
  1 -> GetCounter
  x -> error $ "unknown constructor: " <> show x

serve :: Env -> IO ()
serve rawE = do
  e <- newIORef rawE
  void . runTCPServer (Just "localhost") "8000" $ \s -> do
    msg <- fromBytes <$> recv s 1024
    case msg of
      UpdateCounter -> do
        updateCounter e
      GetCounter -> do
        i <- getCounter e
        sendAll s $ BS.pack [fromIntegral i]

main :: IO ()
main = do
  void . forkIO . serve $ Env 0
  forever $ do
    runTCPClient "localhost" "8000" $ \s -> do
      sendAll s (bin UpdateCounter)
    a <- runTCPClient "localhost" "8000" $ \s -> do
      sendAll s (bin GetCounter)
      recv s 1024
    print (head . BS.unpack $ a)
    -- ans <- recv s 1024
    -- print ans
    threadDelay 3000
