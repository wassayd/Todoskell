{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where


import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Lib 

data Todo = Todo {
    todo :: Text 
   ,isDone :: Bool 
} deriving (Show, Generic, ToJSON, FromJSON)



jsonFile :: FilePath
jsonFile = "todos.json"

exempleTodo :: Todo
exempleTodo = Todo { todo = "learn haskell", isDone = False } 

main :: IO ()
main = do    
    Prelude.putStrLn "Commands:"
    Prelude.putStrLn "+ <String> - Add a TODO entry"
    Prelude.putStrLn "- <Int>    - Delete the numbered entry"
    Prelude.putStrLn "s <Int>    - Show the numbered entry"
    Prelude.putStrLn "e <Int>    - Edit the numbered entry"
    Prelude.putStrLn "l          - List todo"
    Prelude.putStrLn "r          - Reverse todo"
    Prelude.putStrLn "c          - Clear todo"
    Prelude.putStrLn "q          - Quit"
    Prelude.putStrLn "h          - Help"
 

 
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile