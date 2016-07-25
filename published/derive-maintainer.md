# Maintainer required for Derive

_Summary: I'm looking for a maintainer to take over Derive. Interested?_

The [Derive tool](https://github.com/ndmitchell/derive) is a library and set of definitions for generating fragments of code in a formulaic way from a data type. It has a mechanism for [guessing the pattern from a single example](http://ndmitchell.com/downloads/paper-deriving_a_relationship_from_a_single_example-04_sep_2009.pdf), plus a more manual way of writing out generators. It supports 35 generators out of the box, and is a reverse dependency of 75 libraries.

The tool hasn't seen much love for some time, and I no longer use it myself. It requires somewhat regular maintenance to upgrade to new versions of GHC and [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts). There are lots of directions the tool could be taken, more derivations, integration with the GHC Generic derivation system etc. There's a few generic pieces that could be broken off (translation between Template Haskell and haskell-src-exts, the guessing mechanism).

Anyone who is interested should comment on the [GitHub ticket](https://github.com/ndmitchell/derive/issues/6). In the absence of any volunteers I may continue to do the regular upgrade work, or may instead have it taken out of Stackage and stop upgrading it.

