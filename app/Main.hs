{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where


import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, encode, FromJSON, decode )
import qualified Data.ByteString.Lazy as B
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



createTodo :: String -> IO Todo
createTodo str = do
    nbTodo <- countTodos
    return Todo { pos = nbTodo + 1 , todo = str, isDone = False }

displayCommands :: IO ()
displayCommands = do
    putStrLn "Commands:"
    putStrLn "add       <String> - Add a TODO entry"
    putStrLn "delete    <Int>    - Delete the numbered entry"
    putStrLn "show      <Int>    - Show the numbered entry"
    putStrLn "edit      <Int>    - Edit the numbered entry"
    putStrLn "list               - List todo"
    putStrLn "reverse            - Reverse todo"
    putStrLn "clear              - Clear todo"
    putStrLn "quit               - Quit"
    putStrLn "help               - Help"

main :: IO ()
main = do
    displayCommands
    arg <- getLine
    putStrLn $ "Vous avez saisie " ++ arg
    if isArgValid arg
        then
            case arg of
                "add" -> addTodo
                _     -> putStrLn "Une erreur c'est produite"
        else putStrLn "Invalid"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

decodeJSON :: IO (Maybe [Todo])
decodeJSON = do
    jsonData <- getJSON
    let rse = decode jsonData
    return rse




countTodos :: IO Int
countTodos = maybe 0 length <$> decodeJSON

saveTodo :: Todo -> IO ()
saveTodo a = do
    todos <- decodeJSON
    case todos of
        Just todosArr  -> B.writeFile jsonFile $ encode $ todosArr ++ [a]
        Nothing        -> B.writeFile jsonFile $encode [a]



addTodo :: IO ()
addTodo = do
    putStrLn "Ecrire todo fast"
    str <- getLine
    todo <- createTodo str
    saveTodo todo


deleteTodo = undefined

showTodo = undefined

editTodo = undefined

listTodo = undefined

reverseTodo = undefined

clearTodo = undefined