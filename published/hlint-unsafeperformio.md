# HLint now spots bad unsafePerformIO

_Summary: I've just released a new version of HLint that can spot an unsafePerformIO which should have NOINLINE but doesn't._

I've just released [HLint v1.9.10](http://hackage.haskell.org/package/hlint), a tool to suggest improvements to Haskell code. I don't usually bother with release announcements of HLint, as each version just fixes a few things and adds a few hints, it's all in the [changelog](https://github.com/ndmitchell/hlint/blob/master/CHANGES.txt) (plus there have now been 102 releases). The latest release attempts to make using `unsafePerformIO` a little bit safer. A common idiom for [top-level mutable state](http://www.haskell.org/haskellwiki/Top_level_mutable_state) in Haskell is:

    myGlobalVar :: IORef Int
    myGlobalVar = unsafePerformIO (newIORef 17)

That is, define a top-level CAF (function with no variables) and bind it to `unsafePerformIO` of some created mutable state. But the definition above is unsafe. GHC might decide `myGlobalVar` is cheap and inline it into several uses, duplicating the variable so that some functions update different versions. Running this code through the latest version of HLint we see:

    Sample.hs:2:1: Error: Missing NOINLINE pragma
    Found:
      myGlobalVar = unsafePerformIO (newIORef 17)
    Why not:
      {-# NOINLINE myGlobalVar #-}
      myGlobalVar = unsafePerformIO (newIORef 17)

HLint has spotted the problem, and suggested the correct fix.

**Trying it for real**

Let's take the package [`slave-thread-0.1.0`](http://hackage.haskell.org/package/slave-thread-0.1.0) and run HLint on it. Slave thread is a new package that helps you ensure you don't end up with ghost threads or exceptions being thrown but missed - a useful package. Running HLint we see:

    Sample.hs:19:1: Error: Missing NOINLINE pragma
    Found:
      slaves = unsafePerformIO $ Multimap.newIO
    Why not:
      {-# NOINLINE slaves #-}
      slaves = unsafePerformIO $ Multimap.newIO
    
    Sample.hs:20:3: Warning: Redundant $
    Found:
      unsafePerformIO $ Multimap.newIO
    Why not:
      unsafePerformIO Multimap.newIO
    
    Sample.hs:25:1: Error: Eta reduce
    Found:
      fork main = forkFinally (return ()) main
    Why not:
      fork = forkFinally (return ())
    
    Sample.hs:55:28: Warning: Redundant $
    Found:
      PartialHandler.totalizeRethrowingTo_ masterThread $ mempty
    Why not:
      PartialHandler.totalizeRethrowingTo_ masterThread mempty
    
    Sample.hs:72:5: Error: Use unless
    Found:
      if null then return () else retry
    Why not:
      Control.Monad.unless null retry

HLint has managed to spot the missing `NOINLINE` pragma, and also a handful of tiny cleanups that might make the code a little more readable. Fortunately (and independent of HLint), the `NOINLINE` pragma was added in [`slave-thread-0.1.1`](http://hackage.haskell.org/package/slave-thread-0.1.1), so the library no longer suffers from that bug.
