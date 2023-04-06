module Main (main) where

import JsonLibrary (toJsonString, toJsonObject)

main :: IO ()
main = do
    putStrLn "From JsonObject to String:"
    toJsonString
    putStrLn "From String to JsonObject:"
    toJsonObject

