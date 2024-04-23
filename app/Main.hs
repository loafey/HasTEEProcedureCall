{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Binny (Binny (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Data.ByteString qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import Network.Run.TCP (runTCPClient, runTCPServer)
import Network.Socket.ByteString (recv, sendAll)
import RPC (expose)

newtype Env = Env {counter :: Int} deriving (Show)

updateRef :: IORef a -> (a -> a) -> IO ()
updateRef r f = readIORef r >>= writeIORef r . f

updateCounter :: Env -> Int -> Env
updateCounter e i = e{counter = counter e + i}

updateCounter2 :: Env -> Int -> Int -> Env
updateCounter2 e i j = e{counter = counter e + i + j}

getCounter :: Env -> Int
getCounter = counter

$(expose "Env" ["updateCounter", "updateCounter2", "getCounter"])

serve :: Env -> IO ()
serve rawE = do
  e <- newIORef rawE
  void . runTCPServer (Just "localhost") "8000" $ \s -> do
    len <- debin <$> recv s 4
    msg <- debin <$> recv s len

    case msg of
      R'updateCounter i -> do
        updateRef e $ \env -> env{counter = counter env + i}
        sendAll s (BS.singleton 0)
      R'updateCounter2 i j -> do
        updateRef e $ \env -> env{counter = counter env + i + j}
        sendAll s (BS.singleton 0)
      R'getCounter -> do
        r <- readIORef e
        let ans = bin $ counter r
        let ansLen = BS.length ans
        sendAll s (bin ansLen)
        sendAll s ans

acc = serve'Env

main :: IO ()
main = do
  void . forkIO . serve $ Env 0
  let e = Env'R "localhost" "8000"
  forever $ do
    updateCounter'R e 1
    updateCounter2'R e 1 2

    curr <- getCounter'R e
    print curr

    threadDelay 300000
