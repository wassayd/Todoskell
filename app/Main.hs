{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where


import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

data Todo = Todo {
    todo :: Text 
   ,isDone :: Bool 
} deriving (Show, Generic, ToJSON)

meowmers = Todo { todo = "learn haskell", isDone = False } 

main = I.writeFile "todos.json" (encodeToLazyText meowmers)