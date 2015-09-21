# Space Leak Examples

_Summary: Using the technique from the previous post, here are three space leaks it found._

Every large Haskell program almost inevitably contains space leaks. This post examines three space leaks I found while experimenting with a [space-leak detection algorithm](). I have explained the cause behind the first two solutions, but remain mystified by the third.

### Hoogle leak 1

The motivation for looking at space leak detection tools was that [Hoogle v5]() kept having space leaks. In addition, development often reintroduced space leaks. Since Hoogle 5 is run on a virtual machine with only 1Gb of RAM, a space leak will often cause it to use the swap file for the heap, and destroy performance. As a result, I applied the techniques to the `hoogle generate` command (which generates the databases), and the top of the stack was the function:

    xs <- return $ map (second snd) $ sortOn (fst . snd) $ Map.toList $
        Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2))
                         [(s,(p,[t])) | (p,(t,s)) <- zip [0::Int ..] xs]
    storeWrite store TypesDuplicates $ jaggedFromList $ map (reverse . snd) xs
    return $ map fst xs

In order to narrow down the exact line, I inserted `evaluate $ rnf ...` between each line along with `print` statements, which helps narrow down the exact subexpression. For example:

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

It failed after printing `step 2`, and before printing `step 3`. Pulling each subexpression out and repeating the `evaluate $ rnf ...` I reduced the subexpression to:

    Map.fromListWith (\(x1,x2) (y1,y2) -> (min x1 y1, x2 ++ y2)) xs

I was using `Data.Map.Strict` which means for each duplicate element, it applies the above function and then forces the result expression. Unfortunately, the result is a pair, and that only forces the pair itself, not the first component, which is a space leak. I effectively build up `min x1 (min x1 (main x1 ...` in the heap, which would operate faster and take less memory if reduced. I solved the problem with:

    Map.fromListWith (\(x1,x2) (y1,y2) -> (, x2 ++ y2) $! min x1 y1) xs

After that the stack space could be reduced a bit.

### Hoogle leak 2

The next leak appeared in the function:

    spreadNames (reverse . sortOn snd -> xs@((_,limit):_)) = check $ f (99 + genericLength xs) maxBound xs
        where
            check xs | all (isCon . snd) xs && length (nubOrd $ map snd xs) == length xs = xs
                     | otherwise = error "Invalid spreadNames"

            -- I can only assign values between mn and mx inclusive
            f :: Word16 -> Word16 -> [(a, Int)] -> [(a, Name)]
            f !mn !mx [] = []
            f mn mx ((a,i):xs) = (a, Name real) : f (mn-1) (real-1) xs
                where real = fromIntegral $ max mn $ min mx ideal
                      ideal = mn + floor (fromIntegral (min commonNameThreshold i) * fromIntegral (mx - mn) / fromIntegral (min commonNameThreshold limit))


I had already added `!` in the `f` function when writing the function, on the grounds it was likely a candidate for space leaks (an accumulating fold), so was immediately suspicious that I hadn't got it right. However, obvious bang patterns near `real` made no difference, so I tried systematically reducing the problem.

Since this code isn't in `IO`, the `evaluate` technique from the previous leak doesn't work. However, using `seq` does, albeit a bit more fiddly. To check the argument expression (`reverse . sortOn`) wasn't leaking I made the change:

    spreadNames (reverse . sortOn snd -> xs@((_,limit):_)) = evaluate (rnf xs) `seq` trace "passed xs" (check $ f (99 + genericLength xs) maxBound xs)

I was more worried that the GHC optimiser may break the delicate `seq`/`trace` setup, but at `-O0` that didn't happen. Successive attempts at guessing where the problem might be eventually lead to the expression `genericLength xs`, which in my case is instantiated at `Word16`. The definition of `genericLength` reads:

    genericList [] = 0
    genericList (x:xs) = 1 + genericList xs

Alas, a very obvious space leak. In addition, there are two rules:

    {-# RULES ... #-}

If you use `genericLength` on `Int` or `Integer` then it is replaced with a strict version without a space leak. To solve my space leak I replaced `genericLength xs` with `fromIntegral (length xs)` which solved the leak. After that change, the Hoogle test suite can be run with 1Kb of stack - a test that has been added to the continuous integration.

### Shake leak

After solving the space leak from [the original post](), I was then able to run the entire test suite with 1Kb stack on my Windows machine. I made that an RTS option to the Cabal test suite, and my Linux continuous integration started failing. Further experimentation on a Linux machine showed that:

* The entire test failed at 50K, but succeeded at 100K.
* The stack usage could be replicated with only two of the tests - the `tar` test followed by the `benchmark` test. The `tar` test is incredibly simple and likely any of the tests would have worked before `benchmark` to trigger the issue.
* The tests succeeded in 1K if running `benchmark` followed by `tar`.

The initial assumption was that some CAF was being partially evaluated or created by the first test, and then used by the second, but I have yet to see any evidence of that (and I did look). Applying the `-xc` technique suggested a number of possible sites, but the one that eventually lead to a fix was `extractFileTime`, defined as:

    extractFileTime x = ceiling $ modificationTimeHiRes x * 1e4

And called from:

    getFileInfo x = handleBool isDoesNotExistError (const $ return Nothing) $ do
        s <- getFileStatus $ unpackU_ x
        return $ Just (fileInfo $ extractFileTime s, fileInfo $ fromIntegral $ fileSize s)

There is a small (constant sized) space leak here - the result does not force `extractTime`, but returns a pair containing thunks. In fact, `getFileStatus` from the `unix` library allocates a `ForeignPtr` to store `s`, so by not forcing the pair we cause the `ForeignPtr` to live much longer than would be otherwise required. The fix is simple:

    getFileInfo x = handleBool isDoesNotExistError (const $ return Nothing) $ do
        s <- getFileStatus $ unpackU_ x
        a <- evaluate $ fileInfo $ extractFileTime s
        b <- evaluate $ fileInfo $ fromIntegral $ fileSize s
        return $! Just $! (a, b)
 
Afterwards the entire Shake test suite can be run in 1K. Since this is Posix-only code, I understand why the space leak doesn't occur on Windows. What I still don't understand is:

* How does running one test first cause the space leak?
* How does what looks like a small space leak result in over 49K additional stack space?
* Is the fact that `ForeignPtr` is involved behind the scenes somehow relevant?

I would welcome any insights anyone can provide.
