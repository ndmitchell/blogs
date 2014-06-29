# Optimisation with Continuations

_Summary: Continuations are confusing. Here we solve a simple problem (that is at the heart of the Shake build system) using continuations._

Imagine we are given two `IO a` computations, and want to run them both to completion, returning the first `a` value as soon as it is produced (let's ignore exceptions). Writing that in Haskell isn't too hard:

    parallel :: IO a -> IO a -> IO a
    parallel t1 t2 = do
        once <- newOnce
        var <- newEmptyMVar
        forkIO $ t1 >>= once . putMVar var
        forkIO $ t2 >>= once . putMVar var
        readMVar var

We create an empty variable `var` with `newEmptyMVar`, fire off two threads with `forkIO` to run the computations which write their results to `var`, and finish by reading as soon as a value is available with `readMVar`. We use a utility `newOnce` to ensure that only one of the threads calls `putMVar`, defined as:

    newOnce :: IO (IO () -> IO ())
    newOnce = do
        run <- newMVar True
        return $ \act -> do
            b <- modifyMVar run $ \b -> return (False, b)
            when b act

Calling `newOnce` produces a function that given an action will either run it (the first time) or ignore it (every time after). Using `newOnce` we only call `putMVar` for the first thread to complete.

This solution works, and [Shake](https://github.com/ndmitchell/shake#readme) does something roughly equivalent (but much more complex) in it's [main scheduler](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Database.hs#L198). However, this solution has a drawback - it uses two additional threads. Can we use only one additional thread?

For the problem above, running the computations to completion without retrying, you can't avoid two additional threads. To use only one additional thread and run in parallel you must run one of the operations on the calling thread - but if whatever you run on the additional thread finishes first, there's no way to move the other computation off the the calling thread and return immediately. However, we can define:

    type C a = (a -> IO ()) -> IO ()

Comparing `IO a` to `C a`, instead of returning an `a`, we get given a function to pass the `a` to (known as a _continuation_). We still "give back" the `a`, but not as a return value, instead we pass it onwards to a function. We assume that the continuation is called exactly once. We can define `parallel` on `C`:

    parallel :: C a -> C a -> C a
    parallel t1 t2 k = do
        once <- newOnce
        forkIO $ t1 (once . k)
        t2 (once . k)

This definition takes the two computations to run (`t1` and `t2`), plus the continuation `k`. We fork a separate thread to run `t1`, but run `t2` on the calling thread, using only one additional thread. While the `parallel` function won't return until after `t2` completes, subsequent processing using the `a` value will continue as soon as either finishes.

Looking at the [transformers package](http://hackage.haskell.org/package/transformers), we see [Control.Monad.Trans.Cont](http://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Cont.html) contains `ContT`, which is defined as:

    newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}

If we use `r` for `()` and `IO` for `m` then we get the same type as `C`. We can redefine `C` as:

    type C a = ContT () IO a

The changes to `parallel` just involve wrapping with `ContT` and unwrapping with `runContT`:

    parallel :: C a -> C a -> C a
    parallel t1 t2 = ContT $ \k -> do
        once <- newOnce
        forkIO $ runContT t1 (once . k)
        runContT t2 (once . k)

Now we've defined our `parallel` function in terms of `C`, it is useful to convert between `C` and `IO`:

    toC :: IO a -> C a
    toC = liftIO
    
    fromC :: C a -> IO a
    fromC c = do
        var <- newEmptyMVar
        forkIO $ runContT c $ putMVar var
        readMVar var

The `toC` function is already defined by `ContT` as `liftIO`. The `fromC` function needs to change from calling a callback on _any_ thread, to returning a value on this thread, which we can do with a `forkIO` and `MVar`. Given `parallel` on `IO` takes two additional threads, and `parallel` on `C` takes only one, it's not too surprising that converting `IO` to `C` requires an additional thread.

**Aren't threads cheap?**

Threads in Haskell are very cheap, and many people won't care about one additional thread. However, each thread comes with a stack, which takes memory. The stack starts off small (1Kb) and grows/shrinks in 32Kb chunks, but if it ever exceeds 1Kb, it never goes below 32Kb. For certain tasks (e.g. Shake build rules) often some operation will take a little over 1Kb in stack. Since each active rule (started but not finished) needs to maintain a stack, and for huge build systems there can be 30K active rules, you can get over 1Gb of stack memory. While stacks and threads are cheap, they aren't free.

**The plan for Shake**

Shake currently has one thread per active rule, and blocks that thread until all dependencies have rebuilt. The plan is to switch to continuations and only have one thread per rule executing in parallel. This change will not require any code changes to Shake-based build systems, hopefully just reduce memory usage. Until then, huge build systems may wish to pass `+RTS -kc8K`, which can save several 100Mb of memory.
