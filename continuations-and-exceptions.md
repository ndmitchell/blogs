# Continuations and Exceptions

_Summary: In moving Shake to continuations exceptions were the biggest headache. I figured out how to integrate continuations and exception handling._

The [git repo of Shake](https://github.com/ndmitchell/shake) now uses continuations instead of threads, based on the continuations I described in a [previous blog post](http://neilmitchell.blogspot.com/2014/06/optimisation-with-continuations.html). The part that was most difficult was managing exceptions - you can see the resulting code [here](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Monad.hs).

I needed to define a monad where I could capture the continuation, but also define a function to catch exceptions. 

    throwM :: SomeException -> M a
    catchM :: M a -> (SomeException -> M a) -> M a
    captureM :: ((a -> IO ()) -> IO ()) -> M a

So I want to throw exceptions in the monad (which I've called `M`, it's called `RAW` in Shake and does a few extra things which are irrelevant to this post). I want an analogue for the `catch` function, and also want to be able to capture the continuation.

**The first observation** is that since `catchM` must catch any exceptions, since I need to catch those thrown by users calling `error`, then `throwM` can be defined as:

    throwM = liftIO . throwIO

Using `throwIO` gives better guarantees about when the exception is raised, compared to just `throw`.

**The second observation** is that sometimes I want to raise an exception on the continuation, rather than passing back a value. I can build that on top of the `captureM` interface with:

    captureM' :: ((Either SomeException a -> IO ()) -> IO ()) -> M a
    captureM' k = either throwM return =<< captureM k

**The third observation** (which I observed after a few weeks trying not to follow it) is that the continuation may never be called, particularly if the person who was going to call it themselves raises an exception, and if that happens, the `catchM` exception handler will not run. You cannot use `catchM` to implement a robust `finallyM`. I originally tried to come up with schemes to transform the continuations, but it quickly got out of hand, and became an obvious source of bugs.

**The properties** the implementation must meet are the first thing to nail down. I haven't nailed down all the properties, but to a rough approximation:

* `catchM (x >> throwM e) (\_ -> y) >> z === x >> y >> z` -- if you throw an exception inside a `catchM`, you must run the handler.
* `captureM (\k -> x) >>= y === x` -- if you execute something not using the continuation inside `captureM` it must behave like it does outside `captureM`. In particular, if the `captureM` is inside a `catchM`, that `catchM` must not catch the exception.
* `captureM (\k -> k x) >>= y === x >>= y` -- if you capture the continuation then continue that must be equivalent to not capturing the continuation.
* `captureM (\k -> k x >> k x) >>= y === (x >>= y) >> (x >>= y)` -- if you run the continuation twice it must do the same `IO` actions each time. In particular, if the first gets its exceptions caught, the second must do also.

These properties are incomplete (there are other things you expect), and fuzzy (for example, the second property isn't type correct) - but hopefully they give an intuition.

**The implementation** was non-trivial and (sadly) non-elegant. I suspect either my implementation is known in the literature, or a better implementation is known, and I'd welcome a pointer. My scheme is to define the `M` monad as:

    type M a = ContT () (ReaderT (IORef (SomeException -> IO ())) IO) a

Here we have a continuation monad wrapping a reader monad. The reader contains an `IORef` which stores the exception handler. The basic idea is that whenever we start running anything in `M` we call the Haskell `catch` function, and that reads the `IORef` and runs the handler. We can define `catchM` as:

    catchM :: M a -> (SomeException -> M a) -> M a
    catchM m hdl = ContT $ \k -> ReaderT $ \s -> do
        old <- liftIO $ readIORef s
        writeIORef s $ \e -> do
            writeIORef s old
            hdl e `runContT` k `runReaderT` s `catch`
                \e -> ($ e) =<< readIORef s
        flip runReaderT s $ m `runContT` \v -> do
            liftIO $ writeIORef s old
            k v

* We store the previous exception handler as `old`, and insert a new one. After the code has finished (we have left the `catchM` block) we restore the `old` exception handler. In effect, we have a stack of exception handlers.
* When running the handler we pop off the current exception handler by restoring `old`, then since we have already used up our `catch`, we add a new `catch` to catch exceptions in the handler.

We then define `captureM` as:

    captureM :: ((a -> IO ()) -> IO ()) -> M a
    captureM f = ContT $ \k -> ReaderT $ \s -> do
        old <- readIORef s
        writeIORef s throwIO
        f $ \x -> do
            s <- newIORef old
            flip runReaderT s (k x) `E.catch`
                \e -> ($ e) =<< readIORef s
            writeIORef s throwIO

* We make sure to switch the `IORef` back to `throwIO` before we start running the users code, and after we have finished running our code. As a result, if the function that captures the continuation throws an exception it will be raised as normal.
* When running the continuation we create a new `IORef` for the handler, since the continuation might be called twice in parallel, and the separate `IORef` ensures they don't conflict with each other.

Finally, we need a way to run the computation. I've called that `runM`:

    runM :: M a -> (Either SomeException a -> IO ()) -> IO ()
    runM m k = do
        let mm = do
                captureM $ \k -> k ()
                catchM (Right <$> m) (return . Left)
        s <- newIORef throwIO
        mm `runContT` (liftIO . k) `runReaderT` s

The signature of `runM` ends up being the only signature the makes sense given the underlying mechanisms. We define `mm` by using the facilities of `captureM` to insert a `catch` and `catchM` to ensure we never end up in an exception state from `runM`. The rest is just matching up the types.

**Stack depth** can become a problem with this approach. If you regularly do:

    captureM (\k -> k ())

Then each time a `catch` will be wrapped around the function. You can avoid that by implementing `captureM` to get the exception handler first.

    captureM :: ((a -> IO ()) -> IO ()) -> M a
    captureM f = ContT $ \k -> ReaderT $ \s -> do
        old <- readIORef s
        writeIORef s $ \_ ->
            f $ \x -> do
                s <- newIORef old
                flip runReaderT s (k x) `E.catch`
                    \e -> ($ e) =<< readIORef s
        throwIO anyException

Here we unwind the `catch` by doing a `throwIO`, after installing our handler which actually passes the continuation. It is a bit ugly, and I haven't checked if either the `catch` is a problem, or that this solution solves it. 

**The implementation in Shake** has the property that I never call a captured continuation twice, so I implemented it in terms of:

    type M a = ReaderT (SomeException -> IO ()) (ContT () IO) a

I also avoid creating the new `IORef` when invoking the continuation, since I can reuse the existing one. The only real reason to use `ReaderT`/`ContT` over the formulation above would be efficiency (since the one above can model more things), and I haven't benchmarked.

