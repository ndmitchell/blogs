# Safe Library rewrite, generalisation

_Summary: The Safe library now has exact versions of `take`/`drop`, with twelve functions implemented on top of a generalised `splitAt`._ 

The [Safe library](http://hackage.haskell.org/package/safe) is a simple Haskell library that provides versions of standard `Prelude` and `Data.List` functions that usually throw errors (e.g. `tail`), but wrapped to provide better error messages (e.g. `tailNote`), default values (e.g. `tailDef`) and `Maybe` results (e.g. `tailMay`).

I recently released version 0.3.5, which provides a new module `Safe.Exact` containing crashing versions of functions such as `zip`/`zipWith` (which error if the lists are not equal length) and `take`/`drop`/`splitAt` (which error if there are not enough elements), then wraps them to provide safe variants. As an example, the library provides:

    takeExact    :: Int -> [a] -> [a]
    takeExactMay :: Int -> [a] -> Maybe [a]

These are like `take`, but if the `Int` is larger than the length of the list it will throw an error or return `Nothing`. Some sample evaluations:

    takeExactMay 2 [1,2,3] == Just [1,2]
    takeExact    2 [1,2,3] == [1,2]
    takeExactMay 2 [1] == Nothing
    takeExact    2 [1] ==
        1:error "Safe.Exact.takeExact, index too large, index=2, length=1"
    take 1 (takeExact 2 [1]) == [1]

So `takeExactMay` computes up-front whether the whole computation will succeed, and returns a `Nothing` if it will fail. In contrast, `takeExact` produces elements while they are present, but if you demand an additional element that is missing it will result in an error. All the exceptions in the Safe library are designed to provide the maximum level of detail about what went wrong, here telling us the index we were after and the length of the list.

The library provides `takeExact`, `dropExact` and `splitAtExact`, plus `Def`/`May`/`Note` versions, resulting in twelve similar functions. While the implementation of any one function is reasonably short (although not that short, once proper error messages are provided), I didn't want to write the same code twelve times. However, generalising over functions that check up-front and those that check on-demand requires a bit of thought. In the end I settled for:

    splitAtExact_ :: (String -> r) -> ([a] -> r) -> (a -> r -> r) -> Int -> [a] -> r
    splitAtExact_ err nil cons o xs
        | o < 0 = err $ "index must not be negative, index=" ++ show o
        | otherwise = f o xs
        where
            f 0 xs = nil xs
            f i (x:xs) = x `cons` f (i-1) xs
            f i [] = err $
                "index too large, index=" ++ show o ++ ", length=" ++ show (o-i)

Here the `splitAtExact_` function has a parameterised return type `r`, along with three functional arguments that construct and consume the `r` values. The functional arguments are:

* `err :: String -> r`, says how to convert an error into a result value. For up-front checks this produces a `Nothing`, for on-demand checks this calls `error`.
* `nil :: [a] -> r`, says what to do once we have consumed the full number of elements. For `take` we discard all the remaining elements, for `drop` we are only interested in the remaining elements.
* `cons :: a -> r -> r`, says how to deal with one element before we reach the index. For `take` this will be `(:)`, but for functions producing a `Maybe` we have to check the `r` parameter first.

With this generalisation, I was able to write all twelve variants. As a few examples:

    addNote fun msg = error $ "Safe.Exact." ++ fun ++ ", " ++ msg

    takeExact = splitAtExact_ (addNote "takeExact") (const []) (:)
    
    dropExact = splitAtExact_ (addNote "dropExact") id (flip const)

    takeExactMay = splitAtExact_ (const Nothing) (const $ Just []) (\a -> fmap (a:))

    dropExactMay = splitAtExact_ (const Nothing) Just (flip const)

    splitAtExact = splitAtExact_ (addNote "splitAtExact")
        (\x -> ([], x)) (\a b -> first (a:) b)

    splitAtExactMay = splitAtExact_ (const Nothing)
        (\x -> Just ([], x)) (\a b -> fmap (first (a:)) b)

Normally I would have defined `takeExact` and `dropExact` in terms of `fst`/`snd` on top of `splitAtExact`. However, in the Safe library error messages are of paramount importance, so I go to additional effort to ensure the error says `takeExact` and not `splitAtExact`.
