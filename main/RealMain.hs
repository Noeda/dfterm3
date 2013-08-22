module Main ( main ) where

import MockMain
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    dfterm3 $ if null args
      then [ "--websocket=8000"
           , "--websocket-http=8080"
           , "--admin-panel=8081"]
      else args

