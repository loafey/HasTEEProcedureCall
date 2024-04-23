{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Binny (Binny (..))
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

updateCounter :: Int -> IORef Env -> IO ()
updateCounter i = updateRef $ \env -> env{counter = counter env + i}
getCounter :: IORef Env -> IO Int
getCounter = (counter <$>) . readIORef

data Message
  = UpdateCounter Int
  | GetCounter
  deriving (Show, Generic, Binny)

serve :: Env -> IO ()
serve rawE = do
  e <- newIORef rawE
  void . runTCPServer (Just "localhost") "8000" $ \s -> do
    msg <- debin <$> recv s 1024
    case msg of
      UpdateCounter i -> do
        updateCounter i e
      GetCounter -> do
        i <- getCounter e
        sendAll s $ BS.pack [fromIntegral i]

main :: IO ()
main = do
  void . forkIO . serve $ Env 0
  forever $ do
    runTCPClient "localhost" "8000" $ \s -> do
      sendAll s (bin (UpdateCounter 3))
    a <- runTCPClient "localhost" "8000" $ \s -> do
      sendAll s (bin GetCounter)
      recv s 1024
    print . head . BS.unpack $ a
    -- ans <- recv s 1024
    -- print ans
    threadDelay 3000
