{-# LANGUAGE BlockArguments #-}

module Main (main) where
import Cli

main :: IO ()
main = do
     status' <- parseInput
     let status = status'
     print status
--     case status of
--       OK -> putStrLn "All okay!"
--       Md -> putStrLn "Md file created"
--       Failed -> putStrLn "Something went wrong!"
--       Us -> putStrLn "Use file created"
