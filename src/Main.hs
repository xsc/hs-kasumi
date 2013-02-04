module Main where

import System.Environment ( getArgs )
import Kasumi

printHelp = do
    putStrLn "Usage: kasumi [--encode|--decode] [-k <Hex Key>|-f <Key File>] [<File>]"

main = do
    args <- getArgs
    if null args 
       then printHelp
       else putStrLn "NOT IMPLEMENTED"
