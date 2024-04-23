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
import RPC (createRPC)

newtype Env = Env {counter :: Int} deriving (Show)

updateRef :: (a -> a) -> IORef a -> IO ()
updateRef f r = readIORef r >>= writeIORef r . f
readRef :: IORef a -> IO a
readRef = readIORef

updateEnv :: Int -> IORef Env -> IO ()
updateEnv i = updateRef $ \env -> env{counter = counter env + i}

updateCounter :: Env -> Int -> Env
updateCounter e i = e{counter = counter e + i}

updateCounter2 :: Env -> Int -> Int -> Env
updateCounter2 e i j = e{counter = counter e + i + j}

getCounter :: Env -> Int
getCounter = counter

$(createRPC "Env" ["updateCounter", "updateCounter2"])

serve :: Env -> IO ()
serve rawE = do
  e <- newIORef rawE
  void . runTCPServer (Just "localhost") "8000" $ \s -> do
    msg <- debin <$> recv s 1024
    case msg of
      R'updateCounter i -> do
        updateEnv i e
        r <- readIORef e
        print r
        sendAll s (BS.singleton 0)
      R'updateCounter2 i j -> do
        updateEnv (i + j) e
        r <- readIORef e
        print r
        sendAll s (BS.singleton 0)

main :: IO ()
main = do
  void . forkIO . serve $ Env 0
  let e = Env'R "localhost" "8000"
  forever $ do
    updateCounter'R e 1
    updateCounter2'R e 1 2
    -- a <- runTCPClient "localhost" "8000" $ \s -> do
    --   sendAll s (bin GetCounter)
    --   recv s 1024
    -- print . head . BS.unpack $ a
    -- ans <- recv s 1024
    -- print ans
    threadDelay 300000
