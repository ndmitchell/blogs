# Installing the Haskell Network library on Windows

_Summary: This post describes how to install the Haskell network library on Windows, again._

I recently bought a new computer, and tried to install GHC 8.0.1 then upgrade the network library using Cabal. As I have come to expect, it didn't work. Using Git Bash, I got the error:

	$ cabal install network-2.6.3.1
	Resolving dependencies...
	Configuring network-2.6.3.1...
	Failed to install network-2.6.3.1
	Build log ( C:\Users\Neil\AppData\Roaming\cabal\logs\network-2.6.3.1.log ):
	Configuring network-2.6.3.1...
	configure: WARNING: unrecognized options: --with-compiler
	checking for gcc... C:\ghc\GHC-80~1.1┼║
	checking whether the C compiler works... no
	configure: error: in `C:/Neil':
	configure: error: C compiler cannot create executables
	See `config.log' for more details
	cabal: Leaving directory '.'
	cabal.exe: Error: some packages failed to install:
	old-time-1.1.0.3 failed during the configure step. The exception was:
	ExitFailure 77

Running `-v3` shows the `CC` variable is being set to `C:\ghc\GHC-80~1.1┼║`, which looks like a buffer corruption or encoding issue. Fortunately the workaround was relatively easy:

    $ cabal unpack network-2.6.3.1
    $ cd network-2.6.3.1
	$ cabal configure
    ... fails with a similar error to above ...
    $ sh ./configure
    $ cabal build
    $ cabal copy
    $ cabal register

I had to repeat the same pattern for the latest version of `old-time`, and the same pattern worked.
