# Shake in the wild

_Summary: I spotted a few things using Shake, which I had nothing to do with._

In the past few days I have come across several things using the [Shake build system](https://github.com/ndmitchell/shake#readme). I wasn't involved in any of them, and haven't (yet) tried any of them out, but they certainly look cool.

**ToolCabal**

Tibor Bremer from Utrecht University gave a talk at the [Haskell Implementors Workshop 2014](http://www.haskell.org/haskellwiki/HaskellImplementorsWorkshop/2014#Extending_Cabal_with_Plugins.2C_Preprocessors_and_Multi-target_Compilers) about his [ToolCabal](https://github.com/TiborIntelSoft/ToolCabal) project. This project replaces the "build a package" part of Cabal with something more flexible, supporting multiple simultaneous targets and more flexible preprocessors - all built on top of Shake. It doesn't attempt to tackle dependency resolution yet. There is a video of the talk:

<iframe width="420" height="315" src="https://www.youtube.com/embed/VUyIu2T1Qss" frameborder="0" allowfullscreen></iframe>

**Samplecount**

The folks behind [Samplecount](http://samplecount.com/) have written several Shake based things. None are yet on [Hackage](http://hackage.haskell.org/), so I suspect they are somewhat prototypes, but they look like they're already used quite seriously.

* [shake-cabal-build](https://github.com/samplecount/shake-cabal-build) to make it easier to build your Shake build systems with Cabal. Shake build systems need to be compiled with GHC, for which I usually use `ghc --make`, but this project explains how to get things building with Cabal - important if your build system pulls in other libraries.
* [shake-language-c](https://github.com/samplecount/shake-language-c) is a project to simplify building C/C++ projects with Shake. From the docs:
> shake-language-c is a cross-platform build system based on the Shake Haskell library. The focus is on cross-compilation of C, C++ and Objective C source code to various target platforms. Currently supported target platforms are iOS, Android NDK, Google Portable Native Client, MacOS X, Linux and Windows (MinGW). Supported host platforms are MacOS X, Linux and Windows.

* [methcla](https://github.com/samplecount/methcla) is their mobile sound engine, which is built using [this Shake script](https://github.com/samplecount/methcla), which (unsurprisingly) uses shake-language-c and shake-cabal-build.
