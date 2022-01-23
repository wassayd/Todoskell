{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where


import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, encode, FromJSON, decode )
import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.ByteString.Char8  as BS
import Data.String (IsString)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Show.Unicode (ushow)

data Todo = Todo {
    pos     :: Int,
    todo    :: String ,
    isDone  :: Bool
} deriving (Generic, ToJSON, FromJSON)

instance Show Todo where
  show (Todo a b c) = show "Id : " ++ show a ++ " Todo : " ++ show b ++ " IsDone " ++ ushow (if c then  '✅' else  '❌')

instance Eq Todo where
   Todo pos _ _ == Todo pos2 _ _ = pos == pos2


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




main ::IO (Either Todo (Either String ()))
main = do
    displayCommands
    arg <- getLine
    if isArgValid arg
        then
            case arg of
                "add" -> do
                    addTodo
                    return $ Right $ Right ()
                "delete" -> do
                    deleteTodo
                    return $ Right $ Right ()
                "show"  -> Left <$> showTodo
                "edit" -> do
                    editTodo
                    return $ Right $ Right ()
                "list" -> Right . Left <$> listTodo
                "reverse" -> do
                    reverseTodo
                    return $ Right $ Right ()
                "clear" -> do
                    clearTodo
                    return $ Right $ Right ()
                "quit" -> return $ Right $ Right ()
                _     -> return $ Right $ Right ()
        else do  
            return $ Right $ Left "Argument invalid"


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getStrictJSON :: IO BS.ByteString
getStrictJSON = BS.readFile jsonFile

decodeStrictJSON :: IO (Maybe [Todo])
decodeStrictJSON = do
    jsonData <- getStrictJSON
    let rse = decode (B.fromChunks [jsonData])
    return rse

decodeJSON :: IO (Maybe [Todo])
decodeJSON = do
    jsonData <- getJSON
    let rse = decode jsonData
    return rse

countTodos :: IO Int
countTodos = maybe 0 length <$> decodeStrictJSON

saveTodo :: Todo -> IO ()
saveTodo a = do
    todos <- decodeJSON
    case todos of
        Just todosArr  -> B.writeFile jsonFile $ encode $ todosArr ++ [a]
        Nothing        -> B.writeFile jsonFile $ encode [a]

saveTodos :: [Todo] -> IO ()
saveTodos a = do
    todos <- decodeJSON
    case todos of
        Just todosArr  -> B.writeFile jsonFile $ encode $ todosArr ++ a
        Nothing        -> B.writeFile jsonFile $ encode a

addTodo :: IO ()
addTodo = do
    putStrLn "Add your todo :"
    str <- getLine
    todo <- createTodo str
    saveTodo todo


deleteTodo :: IO ()
deleteTodo = do
    todo <- getTodo
    todos <- filter (/=todo) <$> getAllTodoStrict
    clearTodo
    saveTodos todos

deleteTodoById :: Int -> IO ()
deleteTodoById pos = do
    todo <- getTodoById pos
    todos <- filter (/=todo) <$> getAllTodoStrict
    clearTodo
    saveTodos todos

showTodo :: IO Todo
showTodo = getTodo

getTodo :: IO Todo
getTodo = do
    putStrLn "Enter Todo's id"
    todoPos <- getLine
    todos <- getAllTodoStrict
    let todo = (!!0) $ filter (check $ read todoPos) todos
    return todo

getTodoById :: Int -> IO Todo
getTodoById todoPos = do
    todos <- getAllTodoStrict
    let todo = (!!0) $ filter (check todoPos) todos
    return todo

getAllTodos :: IO [Todo]
getAllTodos = Data.Maybe.fromMaybe [] <$> decodeJSON

getAllTodoStrict :: IO [Todo]
getAllTodoStrict = Data.Maybe.fromMaybe [] <$> decodeStrictJSON

check :: Int -> Todo -> Bool
check posCheck (Todo pos _ _) = pos == posCheck

editTodo :: IO ()
editTodo = do
    (Todo pos tods done) <- getTodo
    putStrLn "Modify todo"
    newTodoStr <- getLine
    deleteTodoById pos
    saveTodo Todo {pos = pos, todo = newTodoStr, isDone = done }

listTodo :: IO String
listTodo = fmap show getAllTodoStrict

reverseTodo :: IO ()
reverseTodo = do
    todos <- reverse <$> getAllTodoStrict
    B.writeFile jsonFile $ encode todos

clearTodo :: IO () -- Todo: add validation
clearTodo = B.writeFile jsonFile ""