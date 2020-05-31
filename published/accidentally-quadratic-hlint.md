# HLint `--cross` was accidentally quadratic

_Summary: HLint `--cross` was accidentally quadratic in the number of files._

One of my favourite blogs is [Accidentally Quadratic](https://accidentallyquadratic.tumblr.com/), so when the [Haskell linter HLint](https://github.com/ndmitchell/hlint) suffered such a fate, I felt duty bound to write it up. Most HLint hints work module-at-a-time (or smaller scopes), but there is one hint that can process multiple modules simultaneously - the duplication hint. If you write a sufficiently large repeated fragment in two modules, and pass `--cross`, then this hint will detect the duplication. The actual application of hints is HLint is governed by:

```haskell
applyHintsReal :: [Setting] -> Hint -> [ModuleEx] -> [Idea]
```

Given a list of settings, a list of hints (which gets merged to a single composite `Hint`) and a list of modules, produce a list of ideas to suggest. Usually this function is called in parallel with a single module at a time, but when `--cross` is passed, all the modules being analysed get given in one go.

In [HLint 3](https://neilmitchell.blogspot.com/2020/05/hlint-30.html), `applyHintsReal` became quadratic in the number of modules. When you have 1 module, 1^2 = 1, and everything works fine, but `--cross` suffers a lot. The bug was simple. Given a Haskell list comprehension:

```haskell
[(a,b) | a <- xs, b <- xs]
```

When given the list `xs` of `[1,2]` you get back the pairs `[(1,1),(1,2),(2,1),(2,2)]` - the cross product, which is quadratic in the size of `xs`. The real HLint code didn't look much different:

```haskell
[ generateHints m m'
| m <- ms
, ...
, (nm',m') <- mns'
, ...
]
where
    mns' = map (\x -> (scopeCreate (GHC.unLoc $ ghcModule x), x)) ms
```

We map over `ms` to create `mns'` containing each module with some extra information. In the list comprehension we loop over each module `ms` to get `m`, then for each `m` in `ms`, loop over `mns'` to get `m'`. That means you take the cross-product of the modules, which is quadratic.

**How did this bug come about?** HLint used to work against [`haskell-src-exts` (HSE)](https://hackage.haskell.org/package/haskell-src-exts), but now works against the [GHC parser](https://github.com/digital-asset/ghc-lib). [We migrated the hints one by one](https://neilmitchell.blogspot.com/2019/06/hlints-path-to-ghc-parser.html), changing HLint to thread through both ASTs, and then each hint could pick which AST to use. The patch that [introduced this behaviour](https://github.com/ndmitchell/hlint/commit/0948430ef9b65097d3f1d05fdc66616e22e3e0c6) left `ms` as the HSE AST, and made `mns'` the GHC AST. It should have zipped these two together, so for each module you have the HSE and GHC AST, but accidentally took the cross-product.

**How did we spot it?** [Iustin Pop](https://k1024.org/) filed [a bug report](https://github.com/ndmitchell/hlint/issues/1018) noting that each hint was repeated once per file being checked and performance had got significantly worse, hypothesising it was _O(n^2)_. Iustin was right!

**How did we fix it?** By the time the bug was spotted, the HSE AST had been removed entirely, and both `m` and `m'` were the same type, so [deleting one of the loops](https://github.com/ndmitchell/hlint/commit/31665ab581eafd6792b1b229d75bea493e17780f) was easy. The fix is out in HLint version 3.1.4.

**Should I be using `--cross`?** If you haven't heard about `--cross` in HLint, I don't necessarily suggest you start experimenting with it. The duplicate detection hints are [pretty dubious](https://github.com/ndmitchell/hlint/issues/1009#issuecomment-630103050) and I think most people would be better suited with a real duplicate detection tool. I've had good experiences with [Simian](https://www.harukizaemon.com/simian/) in the past.
