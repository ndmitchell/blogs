# HLint's path to the GHC parser

_Summary: HLint is going to switch to the GHC parser over the next few months. The plan is below._

For some time, HLint has been [accumulating a list](https://github.com/ndmitchell/hlint/issues?q=is%3Aissue+is%3Aopen+label%3Adepends-on-HSE) of those files which are valid GHC Haskell but don't parse with [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts). The list of differences keeps growing. While I have appreciated all the maintainers of `haskell-src-exts`, there have been a fair few of them recently, and the project has felt like it was in maintenance mode, rather than a vibrant project.

To solve this problem, I decided to switch to the GHC parser. However, the GHC parse tree changes significantly with each version of GHC, and HLint needs to support more than one version of GHC. The solution was [`ghc-lib`](https://neilmitchell.blogspot.com/2019/02/announcing-ghc-lib.html) - a decoupling of the GHC API, turning it into a reusable library. As of now, the [latest `haskell-src-exts` maintainer](https://mail.haskell.org/pipermail/haskell-cafe/2019-May/131166.html) has recommended people move to `ghc-lib`.

The plan for `HLint` is tracked in [a GitHub issue](https://github.com/ndmitchell/hlint/issues/645). The first step was to switch so all files are parsed with both `haskell-src-exts` and `ghc-lib` - with a failure if either parser fails - that step has been completed and released (with much work from [Shayne Fletcher](https://blog.shaynefletcher.org/), who is my partner in crime for this transition).

The next step was to abstract over the [`Language.Haskell.HLint3` API](https://hackage.haskell.org/package/hlint-2.1.24/docs/Language-Haskell-HLint3.html) to produce a version that didn't fundamentally rely on the `haskell-src-exts` data types. That has led to the [`Language.Haskell.HLint4` API](https://hackage.haskell.org/package/hlint-2.1.24/docs/Language-Haskell-HLint4.html) which makes things like parsed modules abstract, and removes functions that [Aelve Codesearch](https://codesearch.aelve.com/haskell) showed weren't being used in practice (e.g. functions for approximate `Scope` resolution).

The next release will ship with a 0.1 breaking-change bump and `HLint3` reexporting what is currently `HLint4`. If you think the `HLint4` API does not include necessary functions, please let me know ASAP. After that release, we'll start changing hints one by one to use the GHC parse tree. Once that is complete, we will drop the dependency on `haskell-src-exts` and the project will be complete.

For command line users of `HLint` you should notice greater compatibility with GHC, but relatively little else.
