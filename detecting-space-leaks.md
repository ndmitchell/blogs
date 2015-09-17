# Detecting Space Leaks

_Summary: Here is a technique for detecting space leaks quite easily, it's even found one in the base library._

Every large Haskell program almost inevitably contains space leaks. Space leaks are often difficult to detect, but relatively easy to fix once detected (typically insert a `!`). In conjunction with Tom Ellis, we found a fairly simple method to detect such leaks. These ideas have already detected a space leak in the Haskell/GHC `base` libraries [`maximumBy` function](https://ghc.haskell.org/trac/ghc/ticket/10830), which has now been fixed. For a background on space leaks, see [this article]().

Our approach is based around the observation that most space leaks result in an excess use of stack. If you look for the part of the program that results in the largest stack usage, that is the biggest space leak, and the one that should be fixed first.

### Method

Given a program, and a representative run (e.g. the test suite, a suitable input file):

* Compile the program for profiling, e.g. `ghc --make Main.hs -rtsopts -prof -auto-all`.
* Run the program with a specific stack size, e.g. `./Main +RTS -K100K` to run with a 100Kb stack.
* Increase/decrease the stack size until you have determined the minimum stack for which the program succeeds, e.g. `-K33K`.
* Reduce the stack by a small amount and rerun with `-xc`, e.g. `./Main +RTS -K32K -xc`.
* The `-xc` run will print out the stack trace on every exception, look for the one with `StackOverflow` (likely the last one) and look at the stack trace to determine roughly where the leak is.
* Attempt to fix the space leak, confirm by rerunning with `-K32K`.
* Repeat until the test works with a small stack, typically `-K1K`.
* Add something to your test suite to ensure that if the a space leak is ever introduced then it fails.

### Example 1: Using Shake

Applying these techniques to the Shake test suite, I used the test `./shake-test self test`, which compiles Shake using Shake. Initially it failed at `-K32K`, and the stack trace produced by `-xc` was:

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

Looking at the `generateSummary` function, it does no real processing itself, merely summarizes a list of profile entries - producing a number of summary lines with code such as:

    let f xs = if null xs then "0s" else (\(a,b) -> showDuration a ++ " (" ++ b ++ ")") $ maximumBy (compare `on` fst) xs in
        "* The longest rule takes " ++ f (map (prfExecution &&& prfName) xs) ++
        ", and the longest traced command takes " ++ f (map (prfTime &&& prfCommand) $ concatMap prfTraces xs) ++ "."

Most of the code is `map`, `sort`, `maximumBy` and `sum` in various combinations. By commenting out various lines I was able to still produce the space leak using `maximumBy` alone. By reimplementing `maximumBy` in terms of `foldl'`, the error went away. Small benchmarks show that is a regression in GHC 7.10, which I reported as [GHC ticket 10830](https://ghc.haskell.org/trac/ghc/ticket/10830).

After fixing `maximumBy` I was able to reduce the stack to `-K1K`, and have added such a check to the test suite. While this space leak was not actually problematic in practice (it's rarely used code which isn't performance sensitive), it's nice to fix anyway, and good to have a guarantee of other performance.

### Caveats

Before running this method, I was unaware Shake had a space leak, and while it's a fairly small one.

-O2 and profiling may introduce/remove space leaks

stack omits duplicate elements

no stack trace for the base libraries

there are lots of exceptions before

there are a handful of exceptions after

mapM leaks

does it detect all space leaks? There can be small space leaks (2 or 3 elements) which retain a lot of space, say in a map. 

### Results


**The Problem**

The classic example of a space-leak is:

     foldl (+) 0 [1..n]

This expression, when evaluated at `-O0`, causes a space leak - taking `O(n)` memory. Either using `-O2` or `foldl'` removes the space leak and runs in constant space. Any large code base is likely contain several instances where making some accumulator (or part of an accumulator) strict will improve performance without changing the semantics. The fix is usually simple (add a bang pattern or `seq`), but finding the problem, and checking it is solved, are both tricky.

**Our Solution**

Instead of preventing space leaks, our approach is to detect them. Given any program, it must have a "worst" space leak. If the user is not interested in fixing that space leak, then there's no point going any further. Therefore, all you need to do is detect the biggest space leak after a program completes.

Looking at the evaluation model of Haskell, many space leaks can be detected by looking at the stack. In effect, when evaluating `foldl (+) 0 [1..n]`, GHC builds up the expression `(0 + (1 + (2 + (3 + ...`. When evaluating that expression, GHC ends up with the stack:

    ...
    (+) 3
    (+) 2
    (+) 1
    (+) 0

Here we can see that `(+)` repeats on the stack. Our idea is to detect the largest stack, and report its contents, and consider that to be the most interesting space leak.

**Automating Detection**

Given a program, you can detect the largest stack using the following steps:

* Compile the program for profiling, e.g. `ghc --make Main.hs -rtsopts -prof -auto-all`.
* Run the program with a specific stack size, e.g. `./Main +RTS -K100K` to run with a 100Kb stack.
* Increase/decrease the stack size until you have determined the minimum stack for which the program succeeds, e.g. `-K33K`.
* Reduce the stack by a small amount and rerun with `-xc`, e.g. `./Main +RTS -K32K -xc`.
* The `-xc` run will print out the stack trace on every exception, look for the one with `StackOverflow` (likely the last one) and use that to reduce the stack usage.

Using this approach on the Shake test suite initially failed at `-K32K` but succeeded at `-K33K`. Adding `-xc` showed the problem to be in `Development.Shake.Profile.generateSummary`, and commenting out pieces of this function tracked it down to `maximumBy`, and the GHC bug above. Fixing that instance allows the entire test suite to be run with `-K1K`, suggesting there are no other significant and easily detectable space leaks. There are a number of limitations with this approach:

* Do not turn on `-xc` until you have determined the minimum stack size. A lot of programs throw a lot of exceptions that are caught and happily ignored, which generates a lot of output.
* Some exceptions are caught and rethrown or recovered from, so sometimes the final exception will not be the relevant one.
* It seems `-xc` omits stack traces from inside compiled libraries, and also does not print out duplicate stack entries. As a result, spotting the space leak is harder.

**With Shake**

https://ghc.haskell.org/trac/ghc/ticket/10830 - maximumBy has a space leak.

:prof_ self test +RTS -K32K -xc -RTS
-- at 32K it dies, at 33K it works
Development.Shake.Profile.generateSummary

After that -K1K worked fine.

Do without -xc first, since it generates a lot of traces - one for each exception.

Note that some functions use lots of stack, but it's OK - for instance reverse and mapM.


Our proposed solution is to continually scan the stack as the program executes, and afterwards display the stack which had the most occurrences of any single code pointer.

**Limitations**

Sometimes a space leak contains a single element in a map where the delete isn't forced, so a trivially small stack usage.

Sometimes the largest stack is from `mapM`, which can be tamed (see previous blog post).

**Tweaks**

There are a number of possible tweaks, which probably need changing based on real-world experimentation.

* Maybe what's not important is the high-water mark of occurrences, but just the stack when it was biggest.

Perhaps we can already do that? Use profiling options to see what the stack is on exception, and just keep running with progressively smaller stacks?

    ghc -O0 SpaceLeak.hs -rtsopts -prof -fprof-auto && SpaceLeak +RTS -K10K -xc

In the runtime system, record `Map CodePointer Int` being the number of occurrences of a particularly `CodePointer` (e.g. `+`) in the stack at that point. In the above stack, there would be 4 instances of `(+)`. At the end of the program, you report the high-water mark and the `CodePointer` associated with it.

**Tweaks**

Maybe the length from the first to the last is better.

We record `Map CodePointer (Int,Int)` being the depth of the stack where the first one occurs, and the count. When doing a push, you want to look at the longest length from the bottom to the top, with > 3 on the stack. 

Maybe certain functions, e.g. `>>=` in IO, should be excluded.

**Optimisations**

Stay in lock step with the stack segments.

**With Shake**

https://ghc.haskell.org/trac/ghc/ticket/10830 - maximumBy has a space leak.

:prof_ self test +RTS -K32K -xc -RTS
-- at 32K it dies, at 33K it works
Development.Shake.Profile.generateSummary

After that -K1K worked fine.

Do without -xc first, since it generates a lot of traces - one for each exception.

Note that some functions use lots of stack, but it's OK - for instance reverse and mapM.
