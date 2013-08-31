module Main ( main ) where

import MockMain
import System.Environment

main :: IO ()
main = dfterm3 =<< getArgs

