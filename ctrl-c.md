# Handling Control-C in Haskell

_Summary: The development version of `ghcid` seemed to have some problems with terminating when Control-C was hit, so I investigated and learnt some things._

Given a long-running/interactive console program (e.g. [`ghcid`](https://github.com/ndmitchell/ghcid)), when the user hits Control-C/Ctrl-C the program should abort. In this post I'll describe how that works in Haskell, how it can break, and what asynchronous exceptions have to do with it.

#### What happens when the user hits Ctrl-C?

When the user hits Ctrl-C, GHC raises an async exception of type `UserInterrupt` on the main thread. This happens because GHC [installs an interrupt handler](http://stackoverflow.com/a/2352915/160673) which raises that exception, sending it to the main thread with `throwTo`. If you install your own interrupt handler you won't see this behaviour and will have to handle Ctrl-C yourself.

There [are reports](http://stackoverflow.com/a/7941166/160673) that if the user hits Ctrl-C twice the runtime will abort the program. In my tests, that seems to be a feature of the shell rather than GHC itself - in the Windows Command Prompt no amount of Ctrl-C stops an errant program, in Cygwin a single Ctrl-C works.

#### What happens when the main thread receives `UserInterrupt`?

There are a few options:

* If you are not _masked_ and there is no _exception handler_, the thread will abort, which causes the whole program to finish. This behaviour is the desirable outcome if the user hits Ctrl-C.
 
* If you are running inside an _exception handler_ (e.g. `catch` or `try`) which is capable of catching `UserInterrupt` then the `UserInterrupt` exception will be returned. The program can then take whatever action it wishes, including rethrowing `UserInterrupt` or exiting the program.

* If you are running with exceptions _masked_, then the exception will be delayed until you stop being masked. The most common way to be running while masked is if the code is the second argument to `finally` or one of the first two arguments to `bracket`. Since Ctrl-C will be delayed while the program is masked, you should only do quick things while masked.

#### How might I lose `UserInterrupt`?

The easiest way to "lose" a `UserInterrupt` is to catch it and not rethrow it. Taking a real example from `ghcid`, I sometimes want to check if two paths refer to the same file, and to make that check more robust I call `canonicalizePath` first. This function raises errors in some circumstances (e.g. the directory containing the file does not exist), but is inconsistent about error conditions between OS's, and doesn't document its exceptions, so the safest thing is to write:

    canonicalizePathSafe :: FilePath -> IO FilePath
    canonicalizePathSafe x = canonicalizePath x `catch`
        \(_ :: SomeException) -> return x

If there is any exception, just return the original path. Unfortunately, the `catch` will also catch and discard `UserInterrupt`. If the user hits Ctrl-C while `canonicalizePath` is running the program won't abort. The reason is that `UserInterrupt` is not thrown in response to the code inside the `catch`, so ignoring `UserInterrupt` is the wrong thing to do.

#### What is an async exception?

In Haskell there are two distinct ways to throw exceptions, synchronously and asynchronously. Synchronous exceptions are raised on the calling thread, using functions such as `throw` and `error`. In contrast asynchronous exceptions are raised on a different thread, using `throwTo`.

* You can throw an exception 


#### Is `UserInterrupt` an async exception?

In Haskell, there is a type called `AsyncException`, containing four exceptions - each special in their own way:

* `StackOverflow` - the current thread's stack exceeded its limit.
* `HeapOverflow` - never actually raised.
* `ThreadKilled` - raised by calling `killThread` on this thread. Used when a programmer wants to kill a thread.
* `UserInterrupt` - the one we've been talking about so far, raise on the main thread by the user hitting Ctrl-C.

While these 

In our particular case of `caonicalizePathSafe`, if `canonicalizePath` causes a `StackOverflow`, we probably are happy to take the fallback case, but more likely the stack was already close to the limit and will occur again soon. If the programmer calls `killThread` we should exit, but in `ghcid` I know I won't kill this thread.

#### How can I catch avoid catching async exceptions?

There are two ways to avoid catching async exceptions. Firstly, since we expect `canonicalizePath` to complete quickly, we can just mask all async exceptions:

    canonicalizePathSafe x = mask_ $
        canonicalizePath x `catch` \(_ :: SomeException) -> return x

We are now guaranteed that `catch` will not receive an async exception. Unfortunately, if `canonicalizePath` takes a long time, we might delay Ctrl-C unnecessarily.

Alternatively, we can catch only non-async exceptions:

    canonicalizePathSafe x = catchJust
        (\e -> if async e then Nothing else Just e)
        (canonicalizePath x)
        (\_ -> return x)

    async e = isJust (fromException e :: Maybe AsyncException)

We use `catchJust` to only catch exceptions which aren't of type `AsyncException`, so `UserInterrupt` will not be caught.

#### How to robustly deal with `UserInterrupt`?

I've showed how to make `canonicalizePathSafe` not interfere with `UserInterrupt`, but now I need to audit every piece of code (including library functions) that runs on the main thread to ensure it doesn't catch `UserInterrupt`. That is very fragile. A simpler alternative is to push all computation off the main thread:

    import Control.Concurrent.Extra
    import Control.Exception.Extra

    ctrlC :: IO () -> IO ()
    ctrlC act = do
        bar <- newBarrier
        forkFinally act $ signalBarrier bar
        either throwIO return =<< waitBarrier bar

    main :: IO ()
    main = ctrlC $ ... as before ...

We are using the `Barrier` type from [my previous blog post](), which is available from the [`extra` package](). We create a `barrier`, run the main thread on a forked thread, then marshal completion/exceptions back to the main thread. Since the main thread has no `catch` operations and only a few (audited) functions on it, we can be sure that Ctrl-C will quickly abort the program.

#### What about cleanup?

Now that we've pushed the actions on to a different thread, the `finally` sections on other threads. In general, on program shutdown most cleanup actions are unnecessary. As an example, `ghcid` spawns a copy of `ghci`, but on shutdown the pipes are closed and the `ghci` process exits on its own.

If you want cleanup, and it has to happen, you should arrange for it to happen on the main thread.

#### Disclaimer

I'm certainly not an expert on async exceptions, so corrections welcome. All the above assumes compiling with `-threaded`, but most applies without `-threaded`.
