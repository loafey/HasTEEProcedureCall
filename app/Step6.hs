{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Binny (Binny)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import GHC.Generics (Generic)
import RPC (expose)

newtype Env = Env {counter :: Int} deriving (Show)

updateCounter :: Env -> Int -> Env
updateCounter e i = e{counter = counter e + i}

updateCounter2 :: Env -> Int -> Int -> Env
updateCounter2 e i j = e{counter = counter e + i + j}

getCounter :: Env -> Int
getCounter = counter

$(expose "Env" ["updateCounter", "updateCounter2", "getCounter"])

main :: IO ()
main = do
    void . forkIO . serve'Env $ Env 0
    let e = Env'R "localhost" "8000"
    forever $ do
        updateCounter'R e 1
        updateCounter2'R e 1 2

        curr <- getCounter'R e
        print curr

        threadDelay 300000
