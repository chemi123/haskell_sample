import Control.Monad
import Data.Char
import System.IO

main :: IO ()
main = readFileSample'


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
    withFile "host" ReadMode $ \handle -> do
        contents <- hGetContents handle
        putStr contents
