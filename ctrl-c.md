# Handling Control-C in Haskell

_Summary: The development version of `ghcid` seemed to have some problems with terminating when Control-C was hit, so I investigated and learnt some things._

Given a long-running/interactive console program (e.g. [`ghcid`](https://github.com/ndmitchell/ghcid)), when the user hits Control-C/Ctrl-C the program should abort. In this post I'll describe how that works in Haskell, how it can fail, and what asynchronous exceptions have to do with it.

#### What happens when the user hits Ctrl-C?

When the user hits Ctrl-C, GHC raises an async exception of type `UserInterrupt` on the main thread. This happens because GHC [installs an interrupt handler](http://stackoverflow.com/a/2352915/160673) which raises that exception, sending it to the main thread with `throwTo`. If you install your own interrupt handler you won't see this behaviour and will have to handle Ctrl-C yourself.

There [are reports](http://stackoverflow.com/a/7941166/160673) that if the user hits Ctrl-C twice the runtime will abort the program. In my tests, that seems to be a feature of the shell rather than GHC itself - in the Windows Command Prompt no amount of Ctrl-C stops an errant program, in Cygwin a single Ctrl-C works.

#### What happens when the main thread receives `UserInterrupt`?

There are a few options:

* If you are not _masked_ and there is no _exception handler_, the thread will abort, which causes the whole program to finish. This behaviour is the desirable outcome if the user hits Ctrl-C.
 
* If you are running inside an _exception handler_ (e.g. `catch` or `try`) which is capable of catching `UserInterrupt` then the `UserInterrupt` exception will be returned. The program can then take whatever action it wishes, including rethrowing `UserInterrupt` or exiting the program.

* If you are running with exceptions _masked_, then the exception will be delayed until you stop being masked. The most common way of running while masked is if the code is the second argument to `finally` or one of the first two arguments to `bracket`. Since Ctrl-C will be delayed while the program is masked, you should only do quick things while masked.

#### How might I lose `UserInterrupt`?

The easiest way to "lose" a `UserInterrupt` is to catch it and not rethrow it. Taking a real example from `ghcid`, I sometimes want to check if two paths refer to the same file, and to make that check more robust I call `canonicalizePath` first. This function raises errors in some circumstances (e.g. the directory containing the file does not exist), but is inconsistent about error conditions between OS's, and doesn't document its exceptions, so the safest thing is to write:

    canonicalizePathSafe :: FilePath -> IO FilePath
    canonicalizePathSafe x = canonicalizePath x `catch`
        \(_ :: SomeException) -> return x

If there is any exception, just return the original path. Unfortunately, the `catch` will also catch and discard `UserInterrupt`. If the user hits Ctrl-C while `canonicalizePath` is running the program won't abort. The reason is that `UserInterrupt` is not thrown in response to the code inside the `catch`, so ignoring `UserInterrupt` is the wrong thing to do.

#### What is an async exception?

In Haskell there are two distinct ways to throw exceptions, synchronously and asynchronously.

* _Synchronous exceptions_ are raised on the calling thread, using functions such as `throw` and `error`. The point at which a synchronous exception is raised is explicit and can be relied upon.
* _Asynchronous exceptions_ are raised by a different thread, using `throwTo` and a different thread id. The exact point at which the exception occurs can vary.

#### How is the type `AsyncException` related?

In Haskell, there is a type called `AsyncException`, containing four exceptions - each special in their own way:

* `StackOverflow` - the current thread has exceeded its stack limit.
* `HeapOverflow` - never actually raised.
* `ThreadKilled` - raised by calling `killThread` on this thread. Used when a programmer wants to kill a thread.
* `UserInterrupt` - the one we've been talking about so far, raised on the main thread by the user hitting Ctrl-C.

While these have a type `AsyncException`, that's only a hint as to their intended purpose. You can throw any exception either synchronously or asynchronously. In our particular case of `caonicalizePathSafe`, if `canonicalizePath` causes a `StackOverflow`, we probably are happy to take the fallback case, but likely the stack was already close to the limit and will occur again soon. If the programmer calls `killThread` we should exit, but in `ghcid` we know this thread won't be killed.

#### How can I catch avoid catching async exceptions?

There are several ways to avoid catching async exceptions. Firstly, since we expect `canonicalizePath` to complete quickly, we can just mask all async exceptions:

    canonicalizePathSafe x = mask_ $
        canonicalizePath x `catch` \(_ :: SomeException) -> return x

We are now guaranteed that `catch` will not receive an async exception. Unfortunately, if `canonicalizePath` takes a long time, we might delay Ctrl-C unnecessarily.

Alternatively, we can catch only non-async exceptions:

    canonicalizePathSafe x = catchJust
        (\e -> if async e then Nothing else Just e)
        (canonicalizePath x)
        (\_ -> return x)

    async e = isJust (fromException e :: Maybe AsyncException)

We use `catchJust` to only catch exceptions which aren't of type `AsyncException`, so `UserInterrupt` will not be caught. Of course, this actually avoids catching exceptions of type `AsyncException`, which is only related to async exceptions by a convention not enforced by the type system.

Finally, we can catch only the relevant exceptions:

    canonicalizePathSafe x = canonicalizePath x `catch`
        \(_ :: IOException) -> return x

Unfortunately, I don't know what the relevant exceptions are - on Windows `canonicalizePath` never seems to throw an exception. However, `IOException` seems like a reasonable guess.

#### How to robustly deal with `UserInterrupt`?

I've showed how to make `canonicalizePathSafe` not interfere with `UserInterrupt`, but now I need to audit every piece of code (including library functions I use) that runs on the main thread to ensure it doesn't catch `UserInterrupt`. That is fragile. A simpler alternative is to push all computation off the main thread:

    import Control.Concurrent.Extra
    import Control.Exception.Extra

    ctrlC :: IO () -> IO ()
    ctrlC act = do
        bar <- newBarrier
        forkFinally act $ signalBarrier bar
        either throwIO return =<< waitBarrier bar

    main :: IO ()
    main = ctrlC $ ... as before ...

We are using the `Barrier` type from [my previous blog post](), which is available from the [`extra` package](). We create a `barrier`, run the main action on a forked thread, then marshal completion/exceptions back to the main thread. Since the main thread has no `catch` operations and only a few (audited) functions on it, we can be sure that Ctrl-C will quickly abort the program.

Using version 1.1.1 of the `extra` package we can simplify the code to `ctrlC = join . onceFork`.

#### What about cleanup?

Now we've pushed most actions off the main thread, any `finally` sections are on other threads, and will be skipped if the user hits Ctrl-C. Typically this isn't a problem, as program shutdown automatically cleans all non-persistent resources. As an example, `ghcid` spawns a copy of `ghci`, but on shutdown the pipes are closed and the `ghci` process exits on its own. If we do want robust cleanup of resources such as temporary files we would need to run the cleanup from the main thread, likely using `finally`.

#### Should async exceptions be treated differently?

At the moment, Haskell defines many exceptions, any of which can be thrown either synchronously or asynchronously, but then hints that some are probably async exceptions. That's not a very Haskell-like thing to do. Perhaps there should be a `catch` which ignores exceptions thrown with `throwTo`? Perhaps the sync and async exceptions should be of different types? It seems unfortunate that functions have to care about async exceptions as much as they do.

#### Disclaimer

I'm certainly not an expert on async exceptions, so corrections welcome. All the above assumes compiling with `-threaded`, but most applies without `-threaded`.
