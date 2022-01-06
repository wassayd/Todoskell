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
import Data.String (IsString)

data Todo = Todo {
    pos     :: Int,
    todo    :: String , 
    isDone  :: Bool 
} deriving (Show, Generic, ToJSON, FromJSON)


isArgValid :: (Eq a, IsString a) => a -> Bool
isArgValid "add"      = True 
isArgValid "delete"   = True
isArgValid "show"     = True
isArgValid "edit"     = True
isArgValid "list"     = True
isArgValid "reverse"  = True
isArgValid "clear"    = True
isArgValid "quit"     = True
isArgValid "help"     = True
isArgValid _          = False 

jsonFile :: FilePath
jsonFile = "todos.json"

exempleTodo :: Todo
exempleTodo = Todo { pos = 1, todo = "learn haskell", isDone = False } 

main :: IO ()
main = do    
    Prelude.putStrLn "Commands:"
    Prelude.putStrLn "add       <String> - Add a TODO entry"
    Prelude.putStrLn "delete    <Int>    - Delete the numbered entry"
    Prelude.putStrLn "show      <Int>    - Show the numbered entry"
    Prelude.putStrLn "edit      <Int>    - Edit the numbered entry"
    Prelude.putStrLn "list               - List todo"
    Prelude.putStrLn "reverse            - Reverse todo"
    Prelude.putStrLn "clear              - Clear todo"
    Prelude.putStrLn "quit               - Quit"
    Prelude.putStrLn "help               - Help"
    arg <- Prelude.getLine
    Prelude.putStrLn $ "Vous avez saisie " ++ arg
    if isArgValid arg 
        then Prelude.putStrLn "Valid"
        else Prelude.putStrLn "Invalid"

 
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile


saveTodo ::  ToJSON a => a -> B.ByteString
saveTodo = encode