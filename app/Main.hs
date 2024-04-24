{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Binny (Binny)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Generics (Generic)
import RPC (expose)
import System.Environment (getArgs)

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
  args <- getArgs
  case args of
    ["server"] -> do
      putStrLn "Running a server"
      -- create an Env and share it on the cyberspace
      let e = Env 0

      serve'Env e
    _ -> do
      putStrLn "Running a client"
      -- create a remote Env and use it locally as you would with Env
      let e = Env'R "localhost" "8000"

      forever $ do
        updateCounter'R e 1
        updateCounter2'R e 1 2

        curr <- getCounter'R e
        print curr
        threadDelay 300000
