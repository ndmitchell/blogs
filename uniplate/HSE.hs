
module Main where

import Language.Haskell.Exts
import Data.Char
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe


bad = words "#if #else #endif deriving #ifdef"

main = do
    src <- readFile "../../haskell-src-exts/src/Language/Haskell/Exts/Syntax.hs"
    let ParseOk res = parseModule $ unlines $ filter (flip notElem bad . takeWhile (not . isSpace) . dropWhile isSpace) $ lines src
    let xs = types res
    let good = [a | (a,_,_,True) <- xs]
    writeFile "Types.dot" $ unlines $ ["digraph g {"] ++ [from ++ " -> " ++ t ++ ";" | (from,to,_,_) <- xs, t <- to, from `elem` good, t `elem` good] ++ ["}"]
    print ("Number of types",length good)
    print ("Number of constructors",length [() | (_,_,cs,_) <- xs, c <- cs, let t = [prettyPrint x | TyCon x <- universeBi c], any (`elem` good) t])
    --print xs
    

types :: Module -> [(String, [String], [QualConDecl], Bool)]
types  m = [(a,b,c,f [] b) | (a,b,c) <- lst]
    where
        lst = [(prettyPrint name, nub [prettyPrint x | TyCon x <- universeBi ctors], ctors) | DataDecl _ _ _ name _ ctors _ <- universeBi m]
        lst2 = map (\(a,b,c) -> (a,b)) lst
        
        f seen cs | "Exp" `elem` new = True
                  | null new = False
                  | otherwise = f (seen ++ new) $ concatMap (fromMaybe [] . flip lookup lst2) new
            where new = cs \\ seen