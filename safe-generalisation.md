# Safe Library rewrite, generalisation

A new version of the Safe library is out. There are the same two old modules (Safe and Safe.Foldable), plus a new Safe.Exact module. The idea of Safe.Exact is it takes functions in the Prelude/Data.List which are total, but which arguably shouldn't be, produces a non-total version, then produces several "Safe" equivalents. As an example: (e.g. `take 3 "12"` could reasonably error , creates 

    {-# INLINE splitAtExact_ #-}
    splitAtExact_ :: (String -> r) -> ([a] -> r) -> (a -> r -> r) -> Int -> [a] -> r
    splitAtExact_ err nil cons o xs
        | o < 0 = err $ "index must not be negative, index=" ++ show o
        | otherwise = f o xs
        where
            f 0 xs = nil xs
            f i (x:xs) = x `cons` f (i-1) xs
            f i [] = err $ "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)

Which allows me to define:

    -- |
    -- > takeExact n xs =
    -- >   | n >= 0 && n <= length xs = take n xs
    -- >   | otherwise                = error "some message"
    takeExact :: Int -> [a] -> [a]
    takeExact = splitAtExact_ (addNote "" "takeExact") (const []) (:)
    
    -- |
    -- > dropExact n xs =
    -- >   | n >= 0 && n <= length xs = drop n xs
    -- >   | otherwise                = error "some message"
    dropExact :: Int -> [a] -> [a]
    dropExact = splitAtExact_ (addNote "" "dropExact") id (flip const)
    
    -- |
    -- > splitAtExact n xs =
    -- >   | n >= 0 && n <= length xs = splitAt n xs
    -- >   | otherwise                = error "some message"
    splitAtExact :: Int -> [a] -> ([a], [a])
    splitAtExact = splitAtExact_ (addNote "" "splitAtExact")
        (\x -> ([], x)) (\a b -> first (a:) b)
    
    takeExactNote :: String -> Int -> [a] -> [a]
    takeExactNote note = splitAtExact_ (addNote note "takeExactNote") (const []) (:)
    
    takeExactMay :: Int -> [a] -> Maybe [a]
    takeExactMay = splitAtExact_ (const Nothing) (const $ Just []) (\a -> fmap (a:))
    
    dropExactNote :: String -> Int -> [a] -> [a]
    dropExactNote note = splitAtExact_ (addNote note "dropExactNote") id (flip const)
    
    dropExactMay :: Int -> [a] -> Maybe [a]
    dropExactMay = splitAtExact_ (const Nothing) Just (flip const)
    
    splitAtExactNote :: String -> Int -> [a] -> ([a], [a])
    splitAtExactNote note = splitAtExact_ (addNote note "splitAtExactNote")
        (\x -> ([], x)) (\a b -> first (a:) b)
    
    splitAtExactMay :: Int -> [a] -> Maybe ([a], [a])
    splitAtExactMay = splitAtExact_ (const Nothing)
        (\x -> Just ([], x)) (\a b -> fmap (first (a:)) b)
