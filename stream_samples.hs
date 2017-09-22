import Control.Monad
import Data.Char
import System.IO
import System.Directory
import System.Environment
import Data.List
import Control.Exception

main :: IO ()
main = commandLineArgs


readEachLine :: IO ()
readEachLine = forever $ do
    l <- getLine
    putStrLn $ map toUpper l


getContentsSample :: IO ()
getContentsSample = do
    contents <- getContents
    putStrLn $ map toUpper contents


readShordLines :: IO ()
readShordLines = do
    contents <- getContents
    putStr (shortLinesOnly contents)


readShordLines' :: IO ()
readShordLines' = do interact shortLinesOnly


shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines


respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not palindrome") .
    lines
    where
        isPal :: String -> Bool
        isPal xs = xs == reverse xs


readFileSample :: IO ()
readFileSample = do
    handler <- openFile "host" ReadMode
    contents <- hGetContents handler
    putStr contents
    hClose handler


readFileSample' :: IO ()
readFileSample' = do
    withFile' "host" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents


readFileSample'' :: IO ()
readFileSample'' = do
    contents <- readFile "host"
    putStr contents

-- takes handle closer handler
-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f =
    bracket (openFile name mode)
            (\handle -> hClose handle)
            (\handle -> f handle)


writeFileSample :: IO ()
writeFileSample = do
    contents <- readFile "host"
    writeFile "host.txt" contents


appendFileSample :: IO ()
appendFileSample = do
    line <- getLine
    appendFile "todo.txt" (line ++ "\n")


deleteItem :: IO ()
deleteItem = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                    [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which do you like to delete?"
    numberString <- getLine

    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)

        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")


commandLineArgs :: IO ()
commandLineArgs = do
    args <- getArgs
    progName <- getProgName

    putStrLn "The arguments name are: "
    mapM_ putStrLn args

    putStrLn "The program name is:"
    putStr progName
