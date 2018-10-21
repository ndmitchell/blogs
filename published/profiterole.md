# Announcing Profiterole - GHC Profile Viewer

_Summary: `profiterole` reformats GHC profile reports so they are easier to read._

Do you often work with GHC time profiling reports? Do you find them excessively long and hard to navigate? Profiterole reads standard GHC `.prof` files and generates both textual and HTML reports which are typically more than 10x smaller. As an example compare [HLint profile input](https://gist.github.com/ndmitchell/308cd9a2774873c9a74ee613ae203b65#file-hlint-prof) to [HLint Profiterole output](https://gist.github.com/ndmitchell/ab790bbfa482a70fa2db020fda623309#file-hlint-profiterole-txt).

## Usage

To run, first install (`cabal update && cabal install profiterole`), generate a GHC profile the [normal way](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html), then run:

    profiterole myprogram.prof

Profiterole will generate `myprogram.profiterole.txt` and `myprogram.profiterole.html` - both contain the same information, but the HTML has hyperlinks. There are three columns of numbers:

* `TOT` is the total time spent in any item under this code, what GHC calls inherited time.
* `INH` is the total time spent in the items that Profiterole did not move out to the top level.
* `IND` is the individual time, just like GHC profiles.

For large programs, using `+RTS -P` (instead of the common `-p`) will give more accurate results.

## How it works

Profiterole aims to make the profile shorter by combining common subtrees and lifting them to the root - e.g. if you call `parseFile` from 7 places in the code, instead of having 7 pieces of `parseFile` profiling, Profiterole will give you one. With only 1 place containing `parseFile`, it's easier to optimise `parseFile`, and it's easier to read the code calling it without getting lost in the internals.

## How to profile

Given profile data, different ways of looking at it reveal different insights, and the ones discovered by Profiterole have definitely had value. I tend to use:

* I first use [Profiteur](https://hackage.haskell.org/package/profiteur) to get an overall sense of where the time is going visually. Profiteur lets me orientate myself, but tends to be a little difficult to drill into the details and repeat experiments.
* I then use [Profiterole](https://hackage.haskell.org/package/profiteur) to see if there were any repeated pieces of code Profiteur missed, and then dig into the details using Profiterole.
* Only if I'm really going into the details do I go to the GHC `.prof` output - it's pretty rare.
