# Announcing ghc-lib

On behalf of [Digital Asset](https://www.digitalasset.com/) I'm delighted to announce [ghc-lib](https://github.com/digital-asset/ghc-lib), a repackaging of the GHC API to allow it to be used on different GHC versions. The [GHC API](https://hackage.haskell.org/package/ghc) allows you use the [GHC compiler](https://haskell.org/ghc) as a library, so you can parse, analyze and compile Haskell code.

The GHC API comes pre-installed with GHC, and is tied to that GHC version - if you are using GHC 8.6.3, you get version 8.6.3 of the API, and can't change it. The `ghc-lib` package solves that problem, letting you mix and match versions of the GHC compiler and GHC API. Why might you want that?

* Imagine you are writing a tool to work with several versions of the GHC compiler. The GHC API changes significantly between each version, so doing this would require writing a lot of C preprocessor code to support it. An alternative is to use one version of `ghc-lib` which works across multiple versions of GHC.
* Imagine you are modifying the GHC API or want features from GHC HEAD. With `ghc-lib` you can depend on the revised GHC API, without upgrading the compiler used to build everything, speeding up iteration.

While `ghc-lib` provides the full GHC API, it doesn't contain a runtime system, nor does it create a package database. That means you can't run code produced by `ghc-lib` (no runtime), and compiling off-the-shelf code is very hard (no package database containing the `base` library). What you can do:

* Parse Haskell code, making `ghc-lib` a potential replacement for [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts). See the demo [mini-hlint](https://github.com/ndmitchell/ghc-lib/tree/master/examples/mini-hlint) in the `ghc-lib` repo;
* Compile Haskell code as far as GHC's [Core language](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType), which includes renaming and type checking. See the demo [mini-compile](https://github.com/ndmitchell/ghc-lib/tree/master/examples/mini-compile) in the ghc-lib repo, and the carefully tailored [file it compiles](https://github.com/ndmitchell/ghc-lib/blob/master/examples/mini-compile/test/MiniCompileTest.hs).

The package `ghc-lib` is released [on Hackage](https://hackage.haskell.org/package/ghc-lib), and can be used like any normal package, e.g. `cabal install ghc-lib`. Since `ghc-lib` conflicts perfectly with the GHC API and [`template-haskell`](https://hackage.haskell.org/package/template-haskell), you may wish to [`ghc-pkg hide ghc-lib`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#using-packages) and use the language extension `PackageImports` to do `import "ghc-lib" GHC`. There will be two release streams within the `ghc-lib` name:

* Version 8.8.1 will be the version of ghc-lib produced against the released GHC 8.8.1, once GHC 8.8.1 is released. There is no release against GHC 8.6.3 because we had to make changes to GHC to enable ghc-lib, which were only upstreamed in the last few months.
* Version [0.20190204](http://hackage.haskell.org/package/ghc-lib-0.20190204) is the version of ghc-lib using GHC HEAD on the date 2019-02-04.

We've been developing and using `ghc-lib` internally at [Digital Asset](https://www.digitalasset.com/) for the last six months. The use of `ghc-lib` has enabled us to track GHC HEAD for certain projects, and develop improvements to GHC itself, and then integrate them without requiring us to rebuild all Haskell source code on every step. Smoothing out that development loop has been a massive productivity boon to us.

While this is Digital Asset's first open source project in a while, we have been making lots of contributions behind the scenes - it's no coincidence several of my [recent](https://neilmitchell.blogspot.com/2018/12/ghc-from-bug-to-merge.html) [posts](https://neilmitchell.blogspot.com/2019/01/ghc-from-bug-to-merge-2.html) involve my colleague Shayne. In particular, our engineers have been [behind](https://github.com/ghc-proposals/ghc-proposals/pull/158) [several](https://github.com/ghc-proposals/ghc-proposals/pull/168) [GHC](https://github.com/ghc-proposals/ghc-proposals/pull/190) [proposals](https://github.com/ghc-proposals/ghc-proposals/pull/185).

While I'm announcing the project, much of the work has been done by Shayne Fletcher and Jussi Maki, but neither of them have active blogs just yet!
