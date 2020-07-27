# Which packages does Hoogle search?

_Summary: Hoogle searches packages on Stackage._

Haskell (as of 27 July 2020) has 14,490 packages in the [Hackage package repository](https://hackage.haskell.org/). [Hoogle](https://hoogle.haskell.org/) (the Haskell API search engine) searches 2,463 packages. This post explains which packages are searched, why some packages are excluded, and thus, how you can ensure your package is searched.

The first filter is that Hoogle only searches packages on [Stackage](https://www.stackage.org/). Hoogle indexes any package which is either in the latest Stackage nightly or Stackage LTS, but always indexes the latest version that is on Hackage. If you want a Hoogle search that perfectly matches a given Stackage release, I recommend using the Stackage Hoogle search available from [any snapshot page](https://www.stackage.org/nightly). There are two reasons for restricting to only packages on Stackage:

* I want Hoogle results to be useful. The fact that the package currently builds with a recent GHC used by Stackage is a positive sign that the package is maintained and might actually work for a user who wants to use it. Most of the packages on Hackage probably don't build with the latest GHC, and thus aren't useful search results.
* Indexing time and memory usage is proportional to the number of packages, and somewhat the size of those packages. By dropping over 10 thousand packages we can index more frequently and on more constrained virtual machines. With the recent release of [Hoogle 5.0.18](https://hackage.haskell.org/package/hoogle) the technical limitations on size were removed to enable indexing all of Hackage - but there is still no intention to do so.

There are 2,426 packages in Stackage Nightly, and 2,508 in Stackage LTS, with most overlapping. There are 2,580 distinct packages between these two sources, the [Haskell Platform](https://www.haskell.org/platform/) and a few custom packages Hoogle knows about (e.g. GHC).

Of the 2,580 eligible packages, 77 are executables only, so don't have any libraries to search, leaving 2,503 packages.

Of the remaining packages 2,503, 40 are missing documentation on Hackage, taking us down to 2,463. As for why a package might not have documentation:

* Some are missing documentation because they are very GHC internal and are mentioned but not on Hackage, e.g. ghc-heap.
* Some are Windows only and won't generate documentation on the Linux Hackage servers, e.g. [Win32-notify](https://hackage.haskell.org/package/Win32-notify).
* Some have dependencies not installed on the Hackage servers, e.g. [rocksdb-query](https://hackage.haskell.org/package/rocksdb-query).
* Some have documentation that appears to have been generated without generating a corresponding Hoogle data file, e.g. [array-memoize](https://hackage.haskell.org/package/array-memoize).
* Some are just missing docs entirely on Hackage for no good reason I can see, e.g. [bytestring-builder](https://hackage.haskell.org/package/bytestring-builder).

The Hoogle database is generated and deployed once per day, automatically. Occasionally a test failure or dependency outage will cause generation to fail, but I get alerted, and usually it doesn't get stale by more than a few days. If you [add your package to Stackage](https://github.com/commercialhaskell/stackage#add-your-package) and it doesn't show up on Hoogle within a few days, [raise an issue](https://github.com/ndmitchell/hoogle/issues).
