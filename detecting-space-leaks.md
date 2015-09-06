# Automatically detecting space leaks

_Summary: Below is an approach that might detect space leaks._

Every large Haskell program almost inevitably contains space leaks. Space leaks are often tricky to detect, but relatively easy to fix once detected. While on the train, myself and Tom Ellis were discussing space leaks. We came up with the following approach to detect space leaks, but have not evaluated it or implemented it. Does it work? Is it misguided? Can it be implemented in GHC?

**The Problem**

The classic example of a space-leak is:

     foldl (+) 0 [1..n]

This expression, when evaluated at `-O0`, causes a space leak - taking `O(n)` memory. Either using `-O2` or `foldl'` removes the space leak and runs in constant space. Any large code base is likely contain several instances where making some accumulator (or part of an accumulator) strict will improve performance without changing the semantics. The fix is usually simple (add a bang pattern or `seq`), but finding the problem, and checking it is solved, are both tricky.

**Our Solution**

Instead of preventing space leaks, our approach is to detect them. Given any program, it must have a "worst" space leak. If the user is not interested in fixing that space leak, then there's no point going any further. Therefore, all you need to do is detect the biggest space leak after a program completes.

Looking at the evaluation model of Haskell, a space leak can be detected by looking at the stack. In effect, when evaluating `foldl (+)`, GHC builds up the stack:

    ...
    (+) 3
    (+) 2
    (+) 1
    (+) 0 

Here we can see that `(+)` repeats on the stack. Our proposed solution is to continually scan the stack as the program executes, and afterwards display the stack which had the most occurrences of any single code pointer.

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