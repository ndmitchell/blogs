# Beta testing the Windows Minimal GHC Installer

_Summary: There is now a minimal Windows GHC installer that can install the network library._

Michael Snoyman and I are seeking beta testers for our new Windows GHC installer. The installer is very new, and we're keen to get [feedback](https://github.com/snoyberg/minghc/issues).

* [**Page to download the Minimal GHC 7.8.3 Installer**](https://github.com/snoyberg/minghc#using-the-installer)

One of the standard problems when upgrading Haskell libraries on Windows is that sometimes you have to upgrade a package which requires a configure script - typically the `network` library. When that happens, you have to [take special measures](my_blog), and even then, sometimes the binary will fail at runtime (this happens on about half my machines).

Our installer solves that problem by bundling [GHC](https://www.haskell.org/ghc/), [Cabal](https://www.haskell.org/cabal/) and [MSYS](http://www.mingw.org/wiki/MSYS) in one installer. You can install the network library out of the box without any special configuration and it just works.

Our installer _does not_ provide all the packages included with the [Haskell Platform](https://www.haskell.org/platform/), but it _does_ provide an environment where you can easily install those packages. This minimal installer might be more suitable for developers.

**Why might I want this installer**

The installer is much easier to install than the GHC distribution (has a real installer, sets up the `%PATH%`, includes Cabal and can build the `network` library).

The installer has less libraries than the Haskell Platform installer, but includes MSYS so you can build everything in the Platform. The lack of additional packages and close correspondence with GHC (there is one installer for each GHC version) might make it more suitable for library developers. Once GHC 7.10.1 is released, we hope to make a GHC Minimal Installer available within days.

The installer also has more precise control over what is added to the `%PATH%`. You can add all the binaries it provides, or just add a single binary `minghc-7.8.3` which when run at the command prompt temporarily adds all the installed binaries to the `%PATH%`. Using the switcher, you can easily switch GHC versions.
