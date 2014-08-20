# Continuations and Exceptions

_Summary: In moving Shake to continuations, exceptions were the biggest headache. I figured out how to somewhat integrate continuations and exception handling._

The [git repo for Shake](https://github.com/ndmitchell/shake) now suspends inactive computations by capturing their continuation instead of blocking their thread, based on the continuations I described in a [previous blog post](http://neilmitchell.blogspot.com/2014/06/optimisation-with-continuations.html). The most difficult part was managing exceptions. I needed to define a monad where I could capture continuations and work with exceptions, requiring the definitions:

    data M a = ... deriving (Functor, Applicative, Monad, MonadIO)
    throwM :: SomeException -> M a
    catchM :: M a -> (SomeException -> M a) -> M a
    captureM :: ((a -> IO ()) -> IO ()) -> M a

I'm using `M` as the name of the monad. I want equivalents of `throwIO` and `catch` for `M`, along with a function to capture continuations.

**The first observation** is that since `catchM` must catch any exceptions, including those thrown by users calling `error`, then `throwM` can be defined as:

    throwM = liftIO . throwIO

Using `throwIO` gives better guarantees about when the exception is raised, compared to just `throw`.

**The second observation** is that sometimes I want to raise an exception on the continuation, rather than passing back a value. I can build that on top of `captureM` with:

    captureM' :: ((Either SomeException a -> IO ()) -> IO ()) -> M a
    captureM' k = either throwM return =<< captureM k

**The third observation** (which I observed after a few weeks trying not to follow it) is that the continuation may never be called, and that means you cannot implement a robust `finallyM` function. In particular, if the person who was intending to run the continuation themselves raises an exception, the continuation is likely to be lost. I originally tried to come up with schemes for defining the function passed the continuation to guarantee the continuation was called, but it became messy very quickly.

**The properties** we expect of the implementation, to a rough approximation, include:

* `catchM (x >> throwM e) (\_ -> y) >> z === x >> y >> z` -- if you throw an exception inside a `catchM`, you must run the handler.
* `captureM (\k -> x) >>= y === x` -- if you execute something not using the continuation inside `captureM` it must behave like it does outside `captureM`. In particular, if the `captureM` is inside a `catchM`, that `catchM` must not catch the exception.
* `captureM (\k -> k x) >>= y === x >>= y` -- if you capture the continuation then continue that must be equivalent to not capturing the continuation.
* `captureM (\k -> k x >> k x) >>= y === (x >>= y) >> (x >>= y)` -- if you run the continuation twice it must do the same `IO` actions each time. In particular, if the first gets its exceptions caught, the second must do also.

These properties are incomplete (there are other things you expect), and fuzzy (for example, the second property isn't type correct) - but hopefully they give an intuition.

**The implementation** was non-trivial and (sadly) non-elegant. I suspect a better implementation is known in the literature, and I'd welcome a pointer. My implementation defines `M` as:

    type M a = ContT () (ReaderT (IORef (SomeException -> IO ())) IO) a

Here we have a continuation monad wrapping a reader monad. The reader contains an `IORef` which stores the exception handler. The basic idea is that whenever we start running anything in `M` we call the Haskell `catch` function, and the exception handler forwards to the `IORef`. We can define `catchM` as:

    catchM :: M a -> (SomeException -> M a) -> M a
    catchM m hdl = ContT $ \k -> ReaderT $ \s -> do
        old <- liftIO $ readIORef s
        writeIORef s $ \e -> do
            s <- newIORef old
            hdl e `runContT` k `runReaderT` s `catch`
                \e -> ($ e) =<< readIORef s
        flip runReaderT s $ m `runContT` \v -> do
            s <- ask
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

* We make sure to switch the `IORef` back to `throwIO` before we start running the users code, and after we have finished running our code and switch back to user code. As a result, if the function that captures the continuation throws an exception, it will be raised as normal.
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

**Stack depth** could potentially become a problem with this solution. If you regularly do:

    captureM (\k -> k ())

Then each time a `catch` will be wrapped around the function. You can avoid that by changing `captureM` to throw an exception:

    captureM :: ((a -> IO ()) -> IO ()) -> M a
    captureM f = ContT $ \k -> ReaderT $ \s -> do
        old <- readIORef s
        writeIORef s $ \_ ->
            f $ \x -> do
                s <- newIORef old
                flip runReaderT s (k x) `E.catch`
                    \e -> ($ e) =<< readIORef s
        throwIO anyException

Here we unwind the `catch` by doing a `throwIO`, after installing our exception handler which actually passes the continuation. It is a bit ugly, and I haven't checked if either the `catch` is a problem, or that this solution solves it.

**The implementation in Shake** is a bit different to that described above. In Shake I know that captured continuations are never called more than once, so I
can avoid creating a new `IORef` in `captureM`, and I can reuse the existing one. Since I never change the handler, I can use a slightly less powerful definition of `M`:

    type M a = ReaderT (IORef (SomeException -> IO ())) (ContT () IO) a

The resulting code is [Development.Shake.Monad](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Monad.hs), which implements the `RAW` monad, and also does a few extra things which are irrelevant to this post.

**The cool thing about Haskell** is that I've been able to completely replace the underlying Shake `Action` monad from `StateT`/`IO`, to `ReaderT`/`IO`, to `ReaderT`/`ContT`/`IO`, without ever breaking any users of Shake. Haskell allows me to produce effective and flexible abstractions.
