# Hoogle Searching Overview

_Summary: Hoogle 5 has three interesting parts, a pipeline, database and search algorithm._

The Haskell search engine [Hoogle](https://hoogle.haskell.org) has gone through five major designs, the first four of which are described in [these slides from TFP 2011](https://ndmitchell.com/downloads/slides-hoogle_finding_functions_from_types-16_may_2011.pdf). [Hoogle version 5](https://neilmitchell.blogspot.com/2015/01/hoogle-5-is-coming.html) was designed to be a complete rewrite which simplified the design and allowed it to scale to all of [Hackage](https://hackage.haskell.org). All versions of Hoogle have had some preprocessing step which consumes Haskell definitions, and writes out a data file. They then have the search phase which uses that data file to perform searches. In this post I'll go through three parts -- what the data file looks like, how we generate it, and how we search it. When we consider these three parts, the evolution of Hoogle can be seen as:

* Versions 1-3, produce fairly simple data files, then do an expensive search on top. Fails to scale to large sizes.
* Version 4, produce a very elaborate data files, aiming to search quickly on top. Failed because producing the data file required a lot of babysitting and a long time, so was updated very rarely (yearly). Also, searching a complex data file ends up with a lot of corner cases which have terrible complexity (e.g. `a -> a -> a -> a -> a` would kill the server).
* Version 5, generate very simple data files, then do _O(n)_ but small-constant multiplier searching on top. Update the files daily and automatically. Make search time very consistent.

## Version 5 data file

By version 5 I had realised that deserialising the data file was both time consuming and memory hungry. Therefore, in version 5, the data file consists of chunks of data that can be memory-mapped into `Vector` and `ByteString` chunks using a `ForeignPtr` underlying storage. The OS figures out which bits of the data file should be paged in, and there is very little overhead or complexity on the Haskell side. There is a small index structure at the start of the data file which says where these interesting data structures live, and gives them identity using types. For example, to store information about name search we have three definitions:

```haskell
data NamesSize a where NamesSize :: NamesSize Int
data NamesItems a where NamesItems :: NamesItems (V.Vector TargetId)
data NamesText a where NamesText :: NamesText BS.ByteString
```

Namely, in the data file we have `NamesSize` which is an `Int`, `NamesItems` which is a `Vector TargetId`, and `NamesText` which is a `ByteString`. The `NamesSize` is the maximum number of results that can be returned from any non-empty search (used to reduce memory allocation for the result structure), the `NamesText` is a big string with `\0` separators between each entry, and the `NamesItems` are the identifiers of the result for each name, with as many entries as there are `\0` separators.

The current data file is 183Mb for all of Stackage, of which 78% of that is the information about items (documentation, enough information to render them, where the links go etc - we then GZip this information). There are 21 distinct storage types, most involved with type search.

## Generating the data file

Generating the data file is done in four phases.

Phase 0 downloads the inputs, primarily a `.tar.gz` file containing all `.cabal` files, and another containing all the Haddock Hoogle outputs. These `.tar.gz` files are never unpacked, but streamed through and analysed [using conduit](https://neilmitchell.blogspot.com/2015/07/thoughts-on-conduits.html).

Phase 1 reads through all the `.cabal` files to get metadata about each package - the author, tags, whether it's in Stackage etc. It stores this information in a `Map`. This phase takes about 7s and uses 100Mb of memory.

Phase 2 reads through every definition in every Haddock Hoogle output (the `.txt` files `--hoogle` generates). It loads the entry, parses it, processes it, and writes most of the data to the data file, assigning it a `TargetId`. That `TargetId` is the position of the item in the data file, so it's unique, and can be used to grab the relevant item when we need to display it while searching. During this time we collect the unique deduplicated type signatures and names, along with the `TargetId` values. This phase takes about 1m45s and has about 900Mb of memory at the end. The most important part of phase 2 is [not to introduce a space leak](https://neilmitchell.blogspot.com/2015/09/three-space-leaks.html), since then memory soars to many Gb.

Phase 3 processes the name and type maps and writes out the information used for searching. This phase takes about 20s and consumes an additional 250Mb over the previous phase.

Since generating the data file takes only a few minutes, there is a nightly job that updates the data file at 8pm every night. The job takes about 15 minutes in total, because it checks out a new version of [Hoogle from GitHub](https://github.com/ndmitchell/hoogle), builds it, downloads all the data files, generates a data file, runs the tests, and then restarts the servers.

## Searching

Hoogle version 5 works on the principle that it's OK to be _O(n)_ if the constant is small. For textual search, we have a big flat `ByteString`, and give that to some C code that quickly looks for the substring we enter, favouring complete and case-matching matches. Such a loop is super simple, and at the size of data we are working with (about 10Mb), plenty fast enough.

Type search is inspired by the same principle. We deduplicate types, then for each type, we produce an 18 byte fingerprint. There are about 150K distinct type signatures in Stackage, so that results in about 2.5Mb of fingerprints. For every type search we scan all those fingerprints and figure out the top 100 matches, then do a more expensive search on the full type for those top 100, producing a ranking. For a long time (a few years) I hadn't even bothered doing the second phase of more precise matching, and it still gave reasonable results. (In fact, I _never_ implemented the second phase, but happily [Matt Noonan](https://github.com/matt-noonan) [contributed it](https://github.com/ndmitchell/hoogle/commits?author=matt-noonan).)

A type fingerprint is made up of three parts:

* 1 byte being the arity of the function. `a -> b -> c` would have arity 3.
* 1 byte being the number of constructors/variables in the type signature. `Maybe a -> a` would have a value of 3.
* The three rarest names in the function. E.g. `A -> B -> C -> D` would compare how frequent each of `A`, `B`, `C` and `D` were in the index of functions, and record the 3 rarest. Each name is given a 32 bit value (where 0 is the most common and 2^32 is the rarest).

The idea of arity and number of constructors/variables is to try and get an approximate shape fit to the type being search for. The idea of the rarest names is an attempt to take advantage that if you are searching for `ShakeOptions -> [a] -> [a]` then you probably didn't write `ShakeOptions` by accident -- it provides a lot of signal. Therefore, filtering down to functions that mention `ShakeOptions` probably gives a good starting point.

Once we have the top 100 matches, we can then start considering whether type classes are satisfied, whether type aliases can be expanded, what the shape of the actual function is etc. By operating on a small and bounded number of types we can do much more expensive comparisons than if we had to apply them to every possible candidate.

## Conclusion

Hoogle 5 is far from perfect, but the performance is good, the scale can keep up with the growth of Haskell packages, and the simplicity has kept maintenance low. The technique of operations which are _O(n)_ but with a small constant is one I've applied in other projects since, and I think is an approach often overlooked.
