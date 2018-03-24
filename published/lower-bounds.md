# Adding Package Lower-bounds

_Summary: I hacked Cabal so I could spot where I was missing package lower bounds. The approach has lots of limitations, but I did find one missing lower bound in HLint._

Cabal lets you constrain your dependencies with both upper bounds and lower bounds (for when you are using a feature not available in older versions). While there has been [plenty of debate](http://neilmitchell.blogspot.co.uk/2014/11/upper-bounds-or-not.html) and focus on upper bounds, it feels like lower bounds have been somewhat neglected. As an experiment I decided to modify `cabal` to prefer older versions of packages, then tried to compile a few of my packages. The approach seems sound, but would require a fair bit of work to be generally usable.

**Hacking Cabal**

By default Cabal prefers to choose packages that are already installed and have the highest bound possible. The code to control that is in [`cabal-install/Distribution/Solver/Modular/Preference.hs`](https://github.com/haskell/cabal/blob/5d3618a144991cbe04126abac4e68e7ce1b56df3/cabal-install/Distribution/Solver/Modular/Preference.hs#L87-L127) and reads:

```haskell
-- Prefer packages with higher version numbers over packages with
-- lower version numbers.
latest :: [Ver] -> POption -> Weight
latest sortedVersions opt =
    let l = length sortedVersions
        index = fromMaybe l $ L.findIndex (<= version opt) sortedVersions
    in  fromIntegral index / fromIntegral l
```

To change that to prefer lower versions I simply replaced the final expression with `fromIntegral (l - index) / fromIntegral l`. I also removed the section about giving preferences to currently installed versions, since I wanted the lowest bound to be chosen regardless.

So I didn't mess up my standard copy of Cabal I changed the `.cabal` file to call the executable `kabal`.

**Testing Kabal on Extra**

To test the approach, I used my [`extra` library](https://github.com/ndmitchell/extra), and ran `kabal new-build all`. I used `new-build` to avoid poluting my global package database with these franken-packages, and `all` to build all targets. That failed with:

```console
Failed to build Win32-2.2.2.0.
In file included from dist\build\Graphics\Win32\Window_hsc_make.c:1:0:
Window.hsc: In function 'main':
Window.hsc:189:16: error: 'GWL_USERDATA' undeclared (first use in this function)
C:\ghc\ghc-8.2.2/lib/template-hsc.h:38:10: note: in definition of macro 'hsc_const'
     if ((x) < 0)                                      \
          ^
```

So it seems that Win32-2.2.2.0 claims to work with GHC 8.2, but probably doesn't (unfortunately it's not on the Linux-based [Hackage Matrix](https://matrix.hackage.haskell.org/package/Win32)). We can work around that problem by constraining `Win32` to the version that is already installed with `--constraint=Win32==2.5.4.1`. With that, we can successfully build `extra`. For bonus points we can also use `--enable-test`, checking the test suite has correct lower bounds, which also works.

**Testing Kabal on Shake**

For [Shake](https://shakebuild.com/) we start with:

```console
kabal new-build all --constraint=Win32==2.5.4.1 --enable-test
```

That worked perfectly - either I had sufficient lower bounds, or this approach doesn't do what I hoped...

**Testing Kabal on HLint**

Trying [HLint](https://github.com/ndmitchell/hlint) with our standard recipe we get:

```console
Failed to build ansi-terminal-0.6.2.
[3 of 6] Compiling System.Console.ANSI.Windows.Foreign ( System\Console\ANSI\Windows\Foreign.hs, dist\build\System\Console\ANSI\Windows\Foreign.o )
System\Console\ANSI\Windows\Foreign.hs:90:20: error:
    Ambiguous occurrence `SHORT'
    It could refer to either `System.Win32.Types.SHORT',
                             imported from `System.Win32.Types' at System\Console\ANSI\Windows\Foreign.hs:41:1-25
                          or `System.Console.ANSI.Windows.Foreign.SHORT',
                             defined at System\Console\ANSI\Windows\Foreign.hs:59:1
```

So it seems `ansi-terminal-0.6.2` and `Win32-2.5.4.1` don't cooperate. Let's fix that by restricting `ansi-terminal==0.7` with another constraint. Now we get:

```console
Preprocessing library for cmdargs-0.10.2..
Building library for cmdargs-0.10.2..
[ 1 of 25] Compiling Data.Generics.Any ( Data\Generics\Any.hs, dist\build\Data\Generics\Any.o )

Data\Generics\Any.hs:65:17: error:
    Variable not in scope: tyConString :: TyCon -> String
   |
65 | typeShellFull = tyConString . typeRepTyCon . typeOf
   |                 ^^^^^^^^^^^
```

Oh dear, now it's the fault of `cmdargs`, which is one of my packages! Checking the [Hackage Matrix for `cmdargs`](https://matrix.hackage.haskell.org/package/cmdargs) we see:

INSERT HERE

Namely that 0.10.2 to 0.10.9 don't compile with GHC 8.2. We solve that by going to the maintainers corner and editing the `.cabal` file of released versions to produce a revision with better bounds - replacing `base == 4.*` with `base >= 4 && < 4.10`. Finding the translation from GHC version 8.2 to `base` version 4.10 involved consulting [the magic page of mappings](https://wiki.haskell.org/Base_package#Versions).

After waiting 15 minutes for the package tarballs to update, then doing `cabal update`, I got to a real error in HLint:

```console
src\Hint\Duplicate.hs:44:37: error:
    * Could not deduce (Default (String, String, SrcSpan))
        arising from a use of `duplicateOrdered'
```

Looking at the `data-default` library I see that the `Default` instance for triples was only introduced in version 0.3. Adding the bounds `data-default >= 0.3` to the `hlint.cabal` dependencies fixes the issue, allowing HLint to compile cleanly.

Next, looking at the commit log, I noticed that I'd recently added a lower bound on the `yaml` package. I wondered if I removed that bound then it could be detected?

```console
Resolving dependencies...
Error:
    Dependency on unbuildable library from yaml
    In the stanza 'library'
    In the inplace package 'hlint-2.1'
```

Alas not - Cabal says the library is unbuildable - I don't really know what that means.

**Testing Kabal on Ghcid**

Trying [Ghcid](https://github.com/ndmitchell/ghcid) with our standard recipe and accumulated constraints we get:

```console
Preprocessing library for Win32-notify-0.2..
Building library for Win32-notify-0.2..
[1 of 2] Compiling System.Win32.FileNotify ( dist\build\System\Win32\FileNotify.hs, dist\build\System\Win32\FileNotify.o )

src\System\Win32\FileNotify.hsc:29:9: error:
    Ambiguous occurrence `fILE_LIST_DIRECTORY'
```

So it seems `Win32-notify-0.2` and `Win32-2.5.4.1` don't cooperate. With that discovery I had used up all the time I was willing to spend and stopped the experiment.

**Conclusions**

By modifying Cabal to select for older packages I was able to find and fix a lower bound. However, because all my dependencies aren't lower-bound safe, it became a somewhat manual process. To be practically useful the prinple of correct lower-bounds needs adopting widely. Some notes:

* The [Hackage Matrix](https://matrix.hackage.haskell.org/) provides a large amount of actionable intelligence - a great experience. However, fixing the issues it discovers (actually adding the bounds) is frustratingly manual, requiring lots of clicks and edits in a textbox.
* Using `cabal new-build` caused each directory to gain a `.ghc.environment.x86_64-mingw32-8.2.2` file, which silently recongfigured `ghc` and `ghci` in those directories so they stopped working as I expected. Not a pleasant experience!
* I ran my tests on Windows, and most of the dependencies with incorrect bounds were Windows-specific issues. Maybe Linux would have had less lower-bound issues?
* I used a pretty recent GHC, which excludes a lot of older versions of packages because they don't work on newer GHC versions - picking the oldest-supported GHC would probably have found more bounds.
* Are lower bounds actually useful? If you ignore which packages are globally installed (which both `stack` and `cabal new-build` effectively do) then the only reason to be constrained to an older version is by upper bounds - in which case solving excessive upper-bounds is likely to give more actual benefit.
* I'm not the first person to think of constraining `cabal` to use older versions - e.g. [Cabal bug 2876](https://github.com/haskell/cabal/issues/2876) from 2015.
* The [Trustee tool](http://oleg.fi/gists/posts/2018-01-08-haskell-package-qa.html#s:6) can infer minimum bounds, but it's Linux only so doesn't work for me. It is probably better for people who want to do their own bound checking.
* Compiling `kabal` required a bit of trial and error, I eventually settled on compiling each dependent Cabal package in turn into the global package database, which wasn't ideal, but did work.
