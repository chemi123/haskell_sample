import Control.Monad
import Data.Char
import System.IO
import Control.Exception

main :: IO ()
main = readFileSample''


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
    appendFile "host.txt" (line ++ "\n")
