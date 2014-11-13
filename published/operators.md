# Operators on Hackage

_Summary: I wrote a script to list all operators on Hackage, and which packages they are used by._

In GHC 7.10 the `*>` operator will be moving into the Prelude, which means the Shake library will have to find an alternative operator ([discussion on the mailing list](https://groups.google.com/d/msg/shake-build-system/KUot8nGP6i4/NVRIc_URTEEJ)). In order to pick a sensible operator, I wanted to list all operators in all Hackage packages so I could be aware of clashes.

* [See the list of all exported operators on Hackage](https://gist.github.com/ndmitchell/aa1e0944379a7429cedb#file-gistfile1-txt).
* The most exported operator is `!`, which is exported from 175 packages.
* The package with most exported operators is [plumbers](https://hackage.haskell.org/package/plumbers) which has 931 operators.

Note that exported operators is more than just those defined by the package, e.g. Shake exports the `Eq` class, so `==` is counted as being exported by Shake. However, in most cases, operators exported by a package are defined by that package.

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

After producing the list, I use `groupSort` to produce `[(operator-name, [package])]` then `writeFileUTF8` to produce the output. Running the script takes just over a minute on my ancient computer.

**Writing the code**

Writing the code to produce the operator list took about 15 minutes, and I made some notes as I was going.

* I started by loading up [ghcid](https://hackage.haskell.org/package/ghcid) for the file with the command line `ghcid -t -c "ghci Operators.hs"`. Now every save immediately resulted in a list of warnings/errors, and I never bothered opening the file in `ghci`, I just compiled it to test.
* I started by inserting `take 20 files` so I could debug the script faster and could manually check the output was plausible.
* At first I wrote `takeBaseName src` rather than `takeBaseName file`. That produced a lot of totally incorrect output, woops.
* At first I used `readFile` to suck in the data and `putStr` to print it to the console. That corrupted Unicode operators, so I switched to `readFileUTF8'` and `writeFileUTF8`.
* After switching to `writeFileUTF8` I found a rather serious bug in the extra library, which I [fixed](https://github.com/ndmitchell/extra/commit/1f9f4c9ccd1c5b69826114dbae67be8ba93b23d6) and [added tests for](https://github.com/ndmitchell/extra/commit/c63419d1d1f9e4f6649030c29a71fd896f1b2283), then made a new release.
* After trying to search through the results, I added `(` and `)` around each operator to make it easier to search for the operators I cared about.

**User Exercise**

To calculate the stats of most exported operator and package with most operators I wrote two lines of code - how would you write such code? Hint: both my lines involved `maximumBy`.
