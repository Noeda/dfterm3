module Main ( main ) where

import MockMain
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args
      then dfterm3 [ "--websocket=8000"
                   , "--websocket-http=8080"
                   , "--admin-panel=8081"]
      else dfterm3 args

