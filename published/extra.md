# Announcing the 'extra' library

_Summary: I wrote an `extra` library, which contains lots of my commonly used functions._

When starting to write [Bake](https://github.com/ndmitchell/bake), my first step was to copy a lot of utility functions from [Shake](https://github.com/ndmitchell/shake) - things like `fst3` (select the first element of a triple), `concatMapM` (monadic version of `concatMap`), `withCurrentDirectory` (`setCurrentDirectory` under `bracket`). None of these functions are specific to either Bake or Shake, and indeed, many of the ones in Shake had originally came from [HLint](https://github.com/ndmitchell/hlint). Copy and pasting code is horrible, so I extracted the best functions into a common library which I named [extra](https://github.com/ndmitchell/extra). Unlike the copy/paste versions in each package, I then wrote plenty of tests, made sure the functions worked in the presence of exceptions, did basic performance optimisation and filled in some obvious gaps in functionality.

I'm now using the `extra` library in all the packages above, plus things like [ghcid](https://github.com/ndmitchell/ghcid) and [Hoogle](https://github.com/ndmitchell/hoogle). Interestingly, I'm finding my one-off scripts are making particularly heavy use of the `extra` functions. I wrote this package to reduce my maintenance burden, but welcome other users of `extra`.

My goal for the `extra` library is simple additions to the standard Haskell libraries, just filling out missing functionality, not inventing new concepts. In some cases, later versions of the standard libraries provide the functions I want, so there `extra` makes them available all the way back to GHC 7.2, reducing the amount of CPP in my projects. A few examples:

* [`Control.Monad.Extra.concatMapM`](https://hackage.haskell.org/package/extra/docs/Control-Monad-Extra.html#v:concatMapM) provides a monadic version of `concatMap`, in the same way that `mapM` is a monadic version of `map`.
* [`Data.Tuple.Extra.fst3`](https://hackage.haskell.org/package/extra/docs/Data-Tuple-Extra.html#v:fst3) provides a function to get the first element of a triple.
* [`Control.Exception.Extra.retry`](https://hackage.haskell.org/package/extra/docs/Control-Exception-Extra.html#v:retry) provides a function that retries an `IO` action a number of times.
* [`System.Environment.Extra.lookupEnv`](https://hackage.haskell.org/package/extra/docs/System-Environment-Extra.html#v:lookupEnv) is a function available in GHC 7.6 and above. On GHC 7.6 and above this package reexports the version from `System.Environment` while on GHC 7.4 and below it defines an equivalent version.

The module [`Extra`](https://hackage.haskell.org/package/extra/docs/Extra.html) documents all functions provided by the library, so is a good place to go to see what is on offer. Modules such as `Data.List.Extra` provide extra functions over `Data.List` and also reexport `Data.List`. Users are recommended to replace `Data.List` imports with `Data.List.Extra` if they need the extra functionality.

#### Which functions?

When selecting functions I have been guided by a few principles.

* I have been using most of these functions in my packages - they have proved useful enough to be worth copying/pasting into each project.
* The functions follow the spirit of the original Prelude/base libraries. I am happy to provide partial functions (e.g. `fromRight`), and functions which are specialisations of more generic functions (`whenJust`).
* Most of the functions have trivial implementations. If a beginner couldn't write the function, it probably doesn't belong here.
* I have defined only a few new data types or type aliases. It's a package for defining new utilities on existing types, not new types or concepts.

#### Testing

One feature I particularly like about this library is that the documentation comments are tests. A few examples:

    Just True ||^ undefined  == Just True
    retry 3 (fail "die") == fail "die"
    whenJust (Just 1) print == print 1
    \x -> fromRight (Right x) == x
    \x -> fromRight (Left  x) == undefined

These equalities are more semantic equality than Haskell's value equality. Things with lambda's are run through QuickCheck. Things which print to `stdout` are captured, so the `print 1` test really does a print, which is scraped and compared to the LHS. I run these tests by passing them through a preprocessor, which spits out [this code](https://github.com/ndmitchell/extra/blob/master/test/TestGen.hs), which I then run with some specialised testing functions.
