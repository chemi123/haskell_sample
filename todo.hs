import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command


main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then showUsage
    else do
        let (command:argList) = args
        dispatch command argList


showUsage :: IO ()
showUsage = putStrLn "Usage: ./todo $command arg"


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments!"


view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one argument!"


remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks

    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks

    let number = read numberString
        newTodoTasks = unlines $ delete (todoTasks !! number) todoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)

        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoTasks
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn "The remove command takes exactly two arguments!"


doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command does not exist!"
