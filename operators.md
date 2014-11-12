# Operators on Hackage

_Summary: I wrote a script to list all operators on Hackage, and which packages they are used by._

In GHC 7.10 the `*>` operator will be moving into the Prelude, which means the Shake library will have to find an alternative operator ([discussion on the mailing list](https://groups.google.com/d/msg/shake-build-system/KUot8nGP6i4/NVRIc_URTEEJ)). In order to pick a sensible operator, I wanted to list all operators in all Hackage packages so I could be aware of clashes.

[See the list of all operators on Hackage](https://gist.github.com/ndmitchell/aa1e0944379a7429cedb#file-gistfile1-txt).

**Producing the file**

First I downloaded the [Hoogle databases](http://hackage.haskell.org/packages/hoogle.tar.gz) from Hackage, and extracted them to a directory named `hoogle`. I then ran:

    ghc --make Operators.hs && operators hoogle operators.txt

And uploaded `operators.txt` above. The code for `Operators.hs` is:

    import Control.Exception.Extra
    import Control.Monad
    import Data.List.Extra
    import System.Directory.Extra
    import System.Environment
    import System.FilePath
    import System.IO.Extra
    
    main = do
        [dir,out] <- getArgs
        files <- listFilesRecursive dir
        xs <- forM files $ \file -> do
            src <- readFileUTF8' file `catch_` \_ -> readFile' file `catch_` \_ -> return ""
            return [("(" ++ takeWhile (/= ')') x ++ ")", takeBaseName file) | '(':x <- lines src]
        writeFileUTF8 out $ unlines [unwords $ a : nub b | (a,b) <- groupSort $ concat xs]

This code relies on the normal packages distributed with GHC, plus the [extra](https://hackage.haskell.org/package/extra) package.

**Code explanation**

The script is pretty simple. I first get two arguments, which is where to find the extracted files, and where to write the result. I then use `listFilesRecursive` to recursively find all extracted files, and `forM` to loop over them. For each file I read it in (trying first UTF8, then normal encoding, then giving up). For each line I look for `(` as the first character, and form a list of `[(operator-name, package)]`.

After producing the list, I use `groupSort` to produce `[(operator-name, [package])]` then `writeFileUTF8` to produce the output. Running the script takes roughly a minute on my ancient computer.

**Writing the code**

Writing the code to produce the operator list only took about 15 minutes, and I made notes as I was going. I'm including them here just in case anyone finds them useful.

* I started by loading up [ghcid](https://hackage.haskell.org/package/ghcid) for the file with the command line `ghcid -t -c "ghci Operators.hs"`. Now every save immediately resulted in a list of warnings/errors, and I never bothered opening the file in `ghci`, just compiled it to test.
* I started by inserting `take 20 files` so I could debug the script faster.
* At first I wrote `takeBaseName src` rather than `takeBaseName file`. That produced a lot of totally incorrect output, woops.
* At first I used `readFile` to suck in the data and `putStr` to print it to the console. That didn't work for Unicode operators, so I switched to `readFileUTF8'` and `writeFileUTF8`.
* After switching to `writeFileUTF8` I found a rather serious bug in the extra library, which I [fixed](https://github.com/ndmitchell/extra/commit/1f9f4c9ccd1c5b69826114dbae67be8ba93b23d6) and [added tests for](https://github.com/ndmitchell/extra/commit/c63419d1d1f9e4f6649030c29a71fd896f1b2283), then made a new release.
* After trying to search through the results, I added `(` and `)` around each operator to make it easier to search for the operators I cared about.
