-- RPN
solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:xs) "*" = y * x : xs
          foldingFunction (x:y:xs) "+" = y + x : xs
          foldingFunction (x:y:xs) "-" = y - x : xs
          foldingFunction (x:y:xs) "/" = y / x : xs
          foldingFunction xs numString = read numString : xs


-- dynamic planning
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type AllSections = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]


toLondon :: AllSections
toLondon = [ Section 50 10 30,
             Section 5 90 20,
             Section 40 2 25,
             Section 10 8 0
           ]


calcShorterPath :: (Path, Path) -> Section -> (Path, Path)
calcShorterPath (pathA, pathB) (Section a b c) =
    let toASoFar = sum $ map snd pathA
        toBSoFar = sum $ map snd pathB
        forwardToATime = toASoFar + a
        crossToATime = toBSoFar + b + c
        forwardToBTime = toBSoFar + b
        crossToBTime = toASoFar + a + c
        nextPathA = if forwardToATime <= crossToATime
                        then (A, a) : pathA
                        else (C, c) : (B, b) : pathB
        nextPathB = if forwardToBTime <= crossToBTime
                        then (B, b) : pathB
                        else (C, c) : (A, a) : pathA
    in (nextPathA, nextPathB)


calcOptimalPath :: AllSections -> (Path, Int)
calcOptimalPath allSections =
    let (bestPathToA, bestPathToB) = foldl calcShorterPath ([], []) allSections
        timeToA = sum $ map snd bestPathToA
        timeToB = sum $ map snd bestPathToB
    in if timeToA <= timeToB
           then (reverse bestPathToA, timeToA)
           else (reverse bestPathToB, timeToB)
