# MinGHC is Dead, Long Live Stack

_Summary: The MinGHC project has now finished._

The [MinGHC project](https://github.com/fpco/minghc) was started to produce a minimal Windows installer which didn't contain many packages, but which could install many packages - in particular the `network` package. But time has moved on, and [Stack](https://github.com/commercialhaskell/stack/) now offers everything MinGHC does, but cross-platform and better. To install GHC using Stack, just do `stack setup`, then `stack exec -- my command`. Even if you prefer to use Cabal, `stack exec -- cabal install hlint` is a reasonable approach.

A few final remarks on MinGHC:

* MinGHC was an experiment started by Michael Snoyman, which myself (Neil Mitchell) and Elliot Cameron joined in with. I had fun working with those guys.
* The ideas and approaches behind MinGHC got reused in Stack, so the knowledge learnt has transferred.
* The MinGHC project involved a [Shake build system](https://github.com/fpco/minghc/blob/master/Main.hs) coupled to an [NSIS EDSL installer](https://github.com/fpco/minghc/blob/master/Installer.hs). I think the technologies for building the installer worked out quite nicely.
* The existing MinGHC installers are not going away, but we won't be making any changes, and probably won't upload new versions for GHC 7.10.3.
* It's possible to build Windows installers on a Linux box using a Wine version of NSIS. Very cool.
* I find maintaining such fundamental pieces of an ecosystem, involving installation and system configuration, to be much less fun than just writing Haskell code. Kudos to the Stack and Cabal guys.
