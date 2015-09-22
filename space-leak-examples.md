# Three Space Leaks

_Summary: Using the technique from [the previous post](http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html), here are three space leaks I found._

Every large Haskell program almost inevitably contains space leaks. This post examines three space leaks I found while experimenting with a [space-leak detection algorithm](http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html). The first two space leaks have obvious causes, but I remain mystified by the third.

### Hoogle leak 1

The motivation for looking at space leak detection tools was that [Hoogle 5](http://neilmitchell.blogspot.co.uk/2015/01/hoogle-5-is-coming.html) kept suffering space leaks. Since Hoogle 5 is run on a virtual machine with only 1Gb of RAM, a space leak will often cause it to use the swap file for the heap, and destroy performance. I applied the detection techniques to the `hoogle generate` command (which generates the databases), which told me that `writeDuplicates` took over 100K of stack. The body of `writeDuplicates` is:

    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2))
                         [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    storeWrite store TypesDuplicates $ jaggedFromList $ map (reverse . snd) xs
    return $ map fst xs

I don't expect readers to understand the purpose of the code, but it is interesting to consider if you can spot the space leak, and if you'd have realised so while writing the code.   

In order to narrow down the exact line, I inserted `evaluate $ rnf ...` between each line, along with `print` statements. For example:

    print "step 1"
    evaluate $ rnf xs
    print "step 2"
    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2))
                         [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    evaluate $ rnf xs
    print "step 3"   
    storeWrite store TypesDuplicates $ jaggedFromList $ map (reverse . snd) xs
    print "step 4"
    let res = map fst xs
    evaluate $ rnf res
    print "step 5"
    return res

(Debugging tip: always use `print` for debugging and never for real code, that way getting rid of all debugging output is easy.) It failed after printing `step 2`, but before printing `step 3`. Pulling each subexpression out and repeating the `evaluate`/`rnf` pattern I reduced the expression to:

    Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2)) xs

The `fromListWith` function essentially performs a `foldl` over values with duplicate keys. I was using `Data.Map.Strict`, meaning it the fold was strict, like `foldl'`. However, the result is a pair, so forcing the accumulator only forces the pair itself, not the first component, which contains a space leak. I effectively build up `min x1 (min x1 (min x1 ...` in the heap, which would run faster and take less memory if reduced eagerly. I solved the problem with:

    Map.fromListWith (\(x1,x2) (y1,y2) -> (, x2 ++ y2) $! min x1 y1) xs

After that the stack limit could be reduced a bit. Originally fixed in commit [102966ec](https://github.com/ndmitchell/hoogle/commit/102966ec2b7917e0f4411e94280bd549e3e93878), then refined in [940412cf](https://github.com/ndmitchell/hoogle/commit/940412cf0b08a375ca521c795355681636fdb86c).

### Hoogle leak 2

The next space leak appeared in the function:

    spreadNames (reverse . sortOn snd -> xs@((_,limit):_)) =
        check $ f (99 + genericLength xs) maxBound xs
        where
            check xs | all (isCon . snd) xs && length (nubOrd $ map snd xs) == length xs = xs
                     | otherwise = error "Invalid spreadNames"

            -- I can only assign values between mn and mx inclusive
            f :: Word16 -> Word16 -> [(a, Int)] -> [(a, Name)]
            f !mn !mx [] = []
            f mn mx ((a,i):xs) = (a, Name real) : f (mn-1) (real-1) xs
                where real = fromIntegral $ max mn $ min mx ideal
                      ideal = mn + floor (fromIntegral (min commonNameThreshold i) * fromIntegral (mx - mn) / fromIntegral (min commonNameThreshold limit))


I had already added `!` in the definition of `f` when writing it, on the grounds it was likely a candidate for space leaks (an accumulating map), so was immediately suspicious that I hadn't got it right. However, adding bang patterns near `real` made no difference, so I tried systematically reducing the bug.

Since this code isn't in `IO`, the `evaluate` technique from the previous leak doesn't work. Fortunately, using `seq` works, but is a bit more fiddly. To check the argument expression (`reverse . sortOn`) wasn't leaking I made the change:

    spreadNames (reverse . sortOn snd -> xs@((_,limit):_)) =
        rnf xs `seq` trace "passed xs" (check $ f (99 + genericLength xs) maxBound xs)

I was slightly worried that the GHC optimiser may break the delicate `seq`/`trace` due to imprecise exceptions, but at `-O0` that didn't happen. Successive attempts at testing different subexpressions eventually lead to `genericLength xs`, which in this case returns a `Word16`. The definition of `genericLength` reads:

    genericLength []        =  0
    genericLength (_:l)     =  1 + genericLength l

Alas, a very obvious space leak. In addition, the `base` library provides two rules:

    {-# RULES
      "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
      "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
     #-}

If you use `genericLength` on `Int` or `Integer` then it is replaced with a strict version without a space leak - but on `Word16` the space leak remains. To solve this space leak I replaced `genericLength xs` with `fromIntegral (length xs)` in commit [12c46e93](https://github.com/ndmitchell/hoogle/commit/12c46e93be2e7dbb6e2cfe7f42e707d6be8e511f), which worked. After that change, the Hoogle test suite can be run with 1Kb of stack - a test that has been added to the continuous integration.

### Shake leak

After solving the space leak from [the original post](http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html), I was then able to run the entire test suite with 1Kb stack on my Windows machine. I made that an RTS option to the Cabal test suite, and my Linux continuous integration started failing. Further experimentation on a Linux VM showed that:

* The entire test failed at 50K, but succeeded at 100K.
* The excessive stack usage could be replicated with only two of the tests - the `tar` test followed by the `benchmark` test. The `tar` test is incredibly simple and likely any of the tests before before `benchmark` would have triggered the issue.
* The tests succeeded in 1K if running `benchmark` followed by `tar`.

The initial assumption was that some CAF was being partially evaluated or created by the first test, and then used by the second, but I have yet to find any evidence of that. Applying `-xc` suggested a handful of possible sites (as Shake catches and rethrows exceptions), but the one that eventually lead to a fix was `extractFileTime`, defined as:

    extractFileTime x = ceiling $ modificationTimeHiRes x * 1e4

And called from:

    getFileInfo x = handleBool isDoesNotExistError (const $ return Nothing) $ do
        s <- getFileStatus $ unpackU_ x
        return $ Just (fileInfo $ extractFileTime s, fileInfo $ fromIntegral $ fileSize s)

There is a small (constant sized) space leak here - the result does not force `extractTime`, but returns a pair containing thunks. In fact, `getFileStatus` from the `unix` library allocates a `ForeignPtr` to store `s`, so by not forcing the pair we cause the `ForeignPtr` to live much longer than would be otherwise required. The fix from commit [2ee36a99](https://github.com/ndmitchell/shake/commit/2ee36a99bd3efd8e0293a45d0a837a53adc0ad78) is simple:

    getFileInfo x = handleBool isDoesNotExistError (const $ return Nothing) $ do
        s <- getFileStatus $ unpackU_ x
        a <- evaluate $ fileInfo $ extractFileTime s
        b <- evaluate $ fileInfo $ fromIntegral $ fileSize s
        return $! Just $! (a, b)
 
Afterwards the entire Shake test suite can be run in 1K. Since `getFileInfo` is different on Windows vs Linux, I understand why the space leak doesn't occur on Windows. What I still don't understand is:

* How does running one test first cause the space leak in the second test?
* How does what looks like a small space leak result in over 49K additional stack space?
* Is the fact that `ForeignPtr` is involved behind the scenes somehow relevant?

I welcome any insights.
