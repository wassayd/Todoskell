{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where


import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C

data Todo = Todo {
    todo :: Text 
   ,isDone :: Bool 
} deriving (Show, Generic, ToJSON, FromJSON)


jsonFile :: FilePath
jsonFile = "todos.json"

exempleTodo :: Todo
exempleTodo = Todo { todo = "learn haskell", isDone = False } 

main :: IO ()
main = I.writeFile jsonFile $ encodeToLazyText exempleTodo

 
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

 
decodeJSON :: IO ()
decodeJSON = do json <- getJSON
                Prelude.putStrLn $ C.unpack json