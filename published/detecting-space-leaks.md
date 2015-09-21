# Detecting Space Leaks

_Summary: Below is a technique for easily detecting space leaks. It's even found a space leak in the base library._

Every large Haskell program almost inevitably contains space leaks. Space leaks are often difficult to detect, but relatively easy to fix once detected (typically insert a `!`). Working with Tom Ellis, we found a fairly simple method to detect such leaks. These ideas have detected four space leaks so far, including one in the `base` library [`maximumBy` function](https://ghc.haskell.org/trac/ghc/ticket/10830), which has now been fixed. For an introduction to space leaks, see [this article](https://queue.acm.org/detail.cfm?id=2538488).

Our approach is based around the observation that most space leaks result in an excess use of stack. If you look for the part of the program that results in the largest stack usage, that is the most likely space leak, and the one that should be investigated first.

### Method

Given a program, and a representative run (e.g. the test suite, a suitable input file):

* Compile the program for profiling, e.g. `ghc --make Main.hs -rtsopts -prof -auto-all`.
* Run the program with a specific stack size, e.g. `./Main +RTS -K100K` to run with a 100Kb stack.
* Increase/decrease the stack size until you have determined the minimum stack for which the program succeeds, e.g. `-K33K`.
* Reduce the stack by a small amount and rerun with `-xc`, e.g. `./Main +RTS -K32K -xc`.
* The `-xc` run will print out the stack trace on every exception, look for the one which says `stack overflow` (likely the last one) and look at the stack trace to determine roughly where the leak is.
* Attempt to fix the space leak, confirm by rerunning with `-K32K`.
* Repeat until the test works with a small stack, typically `-K1K`.
* Add something to your test suite to ensure that if a space leak is ever introduced then it fails, e.g. `ghc-options: -with-rtsopts=-K1K` in Cabal.

I have followed these steps for [Shake](https://github.com/ndmitchell/shake), [Hoogle](https://github.com/ndmitchell/hoogle) and [HLint](https://github.com/ndmitchell/hlint), all of which now contain `-K1K` in the test suite or test scripts.

### Example: Testing on Shake

Applying these techniques to the Shake test suite, I used the run `./shake-test self test`, which compiles Shake using Shake. Initially it failed at `-K32K`, and the stack trace produced by `-xc` was:

    *** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace:
      Development.Shake.Profile.generateSummary,
      called from Development.Shake.Profile.writeProfile,
      called from Development.Shake.Core.run.\.\.\,
      called from Development.Shake.Core.run.\.\,
      called from Development.Shake.Database.withDatabase.\,
      called from Development.Shake.Storage.withStorage.continue.\,
      called from Development.Shake.Storage.flushThread,
      called from Development.Shake.Storage.withStorage.continue,
      called from Development.Shake.Storage.withStorage.\,
      called from General.FileLock.withLockFile.\,
      called from General.FileLock.withLockFile,
      called from Development.Shake.Storage.withStorage,
      called from Development.Shake.Database.withDatabase,
      called from Development.Shake.Core.run.\,
      called from General.Cleanup.withCleanup,
      called from Development.Shake.Core.lineBuffering,
      called from Development.Shake.Core.run,
      called from Development.Shake.Shake.shake,
      called from Development.Shake.Args.shakeArgsWith,
      called from Test.Type.shakeWithClean,
      called from Test.Type.shaken.\,
      called from Test.Type.noTest,
      called from Test.Type.shaken,
      called from Test.Self.main,
      called from Test.main,
      called from :Main.CAF:main
    stack overflow

Looking at the `generateSummary` function, it takes complete profile information and reduces it to a handful of summary lines. As a typical example, one line of the output is generated with the code:

    let f xs = if null xs then "0s" else (\(a,b) -> showDuration a ++ " (" ++ b ++ ")") $ maximumBy (compare `on` fst) xs in
        "* The longest rule takes " ++ f (map (prfExecution &&& prfName) xs) ++
        ", and the longest traced command takes " ++ f (map (prfTime &&& prfCommand) $ concatMap prfTraces xs) ++ "."

Most of the code is `map`, `maximum` and `sum` in various combinations. By commenting out pieces I was able to still produce the space leak using `maximumBy` alone. By reimplementing `maximumBy` in terms of `foldl'`, the leak went away. Small benchmarks showed this space leak was a regression in GHC 7.10, which I reported as [GHC ticket 10830](https://ghc.haskell.org/trac/ghc/ticket/10830). To fix Shake, I added the helper:

    maximumBy' cmp = foldl1' $ \x y -> if cmp x y == GT then x else y

After switching to `maximumBy'` I was able to reduce the stack to `-K1K`. While this space leak was not problematic in practice (it's rarely used code which isn't performance sensitive), it's still nice to fix. I modified the Shake test suite to pass `-K1K` so if I ever regress I'll get an immediate notification. (Shake actually had one additional Linux-only space leak, also now fixed, but that's a tale for a future post.)

### Caveats

This method has found several space leaks - two in Shake and two in Hoogle (I also ran it on HLint, which had no failures). However, there are a number of caveats:

* GHC's strictness analyser often removes space leaks by making accumulators strict, so `-O2` tends to remove some space leaks, and profiling may reinsert them by blocking optimisations. I currently check my code using `-O0`, but using libraries I depend on with whatever optimisation they install with by default. By ensuring optimisations do not remove space leaks, it is less likely that minor code tweaks will introduce space leaks due to missed optimisations.
* The stack trace produced by `-xc` omits duplicate adjacent elements, which is often the interesting information when debugging a stack overflow. In practice, it's a little inconvenient, but not terrible. Having GHC provide repetition counts (e.g. `Main.recurse *12`) would be useful.
* The stack traces don't entries for things in imported libraries, which is unfortunate, and often means the location of the error is a 20 line function instead of the exact subexpression. The lack of such information makes fixing leaks take a little longer.
* The `-xc` flag prints stack information on all exceptions, which are often numerous. Lots of `IO` operations make use of exceptions even when they succeed. As a result, it's often easier to run without `-xc` to figure out the stack limit, then turn `-xc` on. Usually the stack overflow exception is near the end.
* There are sometimes a handful of exceptions after the stack overflow, as various layers of the program catch and rethrow the exception. For programs that catch exceptions and rethrow them somewhat later (e.g. Shake), that can sometimes result in a large number of exceptions to wade through. It would be useful if GHC had an option to filter `-xc` to only certain types of exception.
* Some functions in the base libraries are both reasonable to use and have linear stack usage - notably `mapM`. For the case of `mapM` in particular you may wish to switch to [a constant stack version](http://neilmitchell.blogspot.co.uk/2015/09/making-sequencemapm-for-io-take-o1-stack.html) while investigating space leaks.
* This technique catches a large class of space leaks, but certainly not all. As an example, given a `Map Key LargeValue`, if you remove a single `Key` but don't force the `Map`, it will leak a `LargeValue`. When the `Map` is forced it will take only a single stack entry, and thus not be detected as a leak. However, this technique would have detected a [previous Shake space leak](http://neilmitchell.blogspot.co.uk/2013/02/chasing-space-leak-in-shake.html), as it involved repeated calls to `delete`.

### Feedback

If anyone manages to find space leaks using this technique we would be keen to know. I have previously told people that there are many advantages to lazy programming languages, but that space leaks are the big disadvantage. With the technique above, I feel confident that I can now reduce the number of space leaks in my code.
