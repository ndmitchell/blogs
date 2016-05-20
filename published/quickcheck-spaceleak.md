# Another space leak: QuickCheck edition

_Summary: QuickCheck had a space leak in `property`, now fixed._ 

Using the techniques described [in my previous blog post](http://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html) I found another space leak, this time in QuickCheck, which [has now been fixed](https://github.com/nick8325/quickcheck/pull/93). Using QuickCheck we can chose to "label" certain inputs, for example:

```
$ quickCheck $ \p -> label (if p > 0 then "+ve" else "-ve") True
+++ OK, passed 100 tests:
54% -ve
46% +ve
```

Here we `label` numbers based on their value, and at the end QuickCheck tells us how many were in each set. As you might expect, the underlying QuickCheck implementation contains a `Map String Int` to record how many tests get each label.

Unfortunately, the implementation in QuickCheck-2.8.1 has a space leak, meaning that the memory usage is proportional to the number of tests run. We can provoke such a space leak with:

```
quickCheckWithResult stdArgs{maxSuccess=10000} $ \(p :: Double) -> label "foo" True
```

When running with `ghc --make Main.hs -rtsopts && Main +RTS -K1K` we get the error:

```
Main: Stack space overflow: current size 33624 bytes.
```

Using `-K1K` we have detected when we evaluate the space leak, at the end of the program, when trying to print out the summary statistics. The approach taken by `QuickCheck` for `label` is to generate a separate `Map String Int` per run, then at each step merge these `Map` values together using `unionWith (+)`. As such, there are two obvious culprits for the space leak:

* Perhaps the `Map` is not evaluated strictly so in memory we have `unionWith (+) x1 $ unionWith (+) x2 $ unionWith (+) x3 $ ...`. 
* Perhaps the values in the `Map` are not evaluated, so in memory we have `Map {"foo" = 1 + 1 + 1 + ...}`.

QuickCheck avoids the first space leak by keeping its intermediate state in a record type with a strict field for the `Map`. QuickCheck suffers from the second problem. As usual, actually fixing the space leak was easy - just switch from importing `Data.Map` to `Data.Map.Strict`. The `Strict` module ensures that the computations passed to `unionWith` are forced before it returns, and the memory usage remains constant, not linear in the number of tests.

I detected this space leak because the Shake test suite runs with `-K1K` and when running one particular test on a Mac with GHC 8.0 in profiling mode it [caused a stack overflow](https://github.com/ndmitchell/shake/issues/450) - I did not diagnose which of those factors was the ultimate cause.

Many space leaks are now easy to detect (using `-K1K`), moderate difficulty to debug (using the [`-xc` technique](http://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html) or just by eye) and usually easy to fix.