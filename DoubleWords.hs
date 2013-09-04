
module DoubleWords(main) where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.Environment


main = do
    files <- getArgs
    when (null files) $ error "Enter list of files to check"
    putStrLn "Checking for duplicates"
    bad <- fmap concat $ forM files $ \file -> do
        src <- readFile file
        return $ map ((,) file) $ dupes $ worder src
    putStr $ unlines $ if null bad then ["Success"] else
        [file ++ ":" ++ show line ++ ": " ++ word | (file,(line,word)) <- bad] ++
        ["FAILURES (" ++ show (length bad) ++ ")"]


worder :: String -> [(Int, String)]
worder str = [(i,s)
    | (i, s) <- zip [1..] $ lines str
    , s <- words $ concatMap (\x -> if isAlpha x then [toLower x] else if isSpace x then " " else " 1 2 ") s
    , s `notElem` ["l","c","v"]
    ]


dupes :: Eq v => [(k, v)] -> [(k, v)]
dupes = map head . filter ((> 1) . length) . groupBy ((==) `on` snd)
