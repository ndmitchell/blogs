# Continuations and Exceptions

_Summary: In moving Shake to continuations exceptions were the biggest headache. I figured out a number of techniques for making that easier._

The [git repo of Shake](https://github.com/ndmitchell/shake) now uses continuations instead of threads, based on the continuations I described in a [previous blog post](http://neilmitchell.blogspot.com/2014/06/optimisation-with-continuations.html). The part that was most difficult was managing exceptions and resource finalisation. In this post I discuss the two solutions I came up with.

### Resource finalisation

Since I can't rely on the continuations exception handler being called, I instead have a different mechanism for implementing the places where Shake needs it. It's quite rare, only when the user calls `actionFinally` or `actionOnException`, which is rare. Since the functionality is used rarely, and I make no guarantees 

For that I have a Cleaner. Quite simple:

    withCleanup :: (Cleanup -> IO a) -> IO a
    
    addCleanup :: Cleanup -> IO () -> IO (Bool -> IO ())

### Continuations with exceptions

For Shake, I wanted to define a monad where I could capture the continuation, but also define a function to catch exceptions. The operations are implemented in [Monad.hs](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Monad.hs), and in particular I was interested in:

    throwM :: SomeException -> M a
    catchM :: M a -> (SomeException -> M a) -> M a
    captureM :: ((a -> IO ()) -> IO ()) -> M a

So I want to throw exceptions in the monad (which I've called `M`, it's called `RAW` in Shake and does a few extra things which are irrelevant to this post). I want an analogue for the `catch` function, and also want to be able to capture the closure.

**The first observation** is that since `catchM` must catch any exceptions, even those thrown by `error`, then `throwM` can be defined as:

    throwM = liftIO . throwIO

I could equally have define `throwM = throw`, but using `throwIO` gives better guarantees about when the exception is raised.

**The second observation** is that sometimes I want to raise an exception on a continuation. I could do that by passing an exception to the continuation from `captureM`, and hoping the caller evaluates the `a`, but it's much cleaner and safer to instead make the building block:

    captureM :: ((Either SomeException a -> IO ()) -> IO ()) -> M a

This `captureM` can be defined in terms of the previous one as:

    captureM k = either throwM return =<< captureM_original k

**The third observation** (which I observed after a few weeks trying not to follow it) is that the continuation may never be called, particularly if the person who was going to call it themselves raises an exception, and if that happens, the `catchM` exception handler will not run. You cannot use `catchM` to implement a robust `finallyM`. I originally tried to come up with schemes to transform the continuations, but it quickly got out of hand, and became an obvious source of bugs.

**The properties** the implementation must meet are the first thing to nail down. I haven't nailed down all the properties, but basically (ignoring the exception arguments):

    catchM (x >> throwM) >> y  === x >> y
    capture (\k -> x) >> y     === x
    capture (\k -> k x) >>= y  === x >>= y
    capture (\k -> k x >> k x) === (x >>= y) >> (x >>= y)

**The implementation** was non-trivial and (sadly) non-elegant. I suspect either my implementation is known in the literature, or a better implementation is known, and I'd welcome a pointer. My scheme is to define the `M` monad as:

    type M a = ReaderT (IORef (SomeException -> IO ())) (ContT () IO) a

Now we can define:



With these observations, I was able to come up with an implementation 


While I want to throw 

The underlying Shake monad is implemented in the file [Monad.hs](). It defines a type `RAW`, which has a `Monad` instance, and supports both a reader

You can have exception handling using the IORef to store the handler. It seems to work quite nicely.
