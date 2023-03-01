{-# LANGUAGE BlockArguments #-}

module Main (main) where
import Cli

main :: IO ()
main = do
     status' <- parseInput
     let status = status'
     print status
