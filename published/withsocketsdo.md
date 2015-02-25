# Making withSocketsDo unnecessary

_Summary: Currently you have to call `withSocketsDo` before using the Haskell network library. In the next version you won't have to._

The [Haskell network library](https://hackage.haskell.org/package/network) has always had a weird and unpleasant invariant. Under Windows, you must call [`withSocketsDo`](https://hackage.haskell.org/package/network/docs/Network.html#v:withSocketsDo) before calling any other functions. If you forget, the error message isn't particularly illuminating (e.g. getAddrInfo, does not exist, error 10093). Calling `withSocketsDo` isn't harmful under Linux, but equally isn't necessary, and thus easy to accidentally omit. The network library has recently [merged some patches](https://github.com/haskell/network/pull/158) so that in future versions there is no requirement to call `withSocketsDo`, even on Windows.

**Existing versions of network**

The reason for requiring `withSocketsDo` is so that the network library can initialise the [Windows Winsock library](http://en.wikipedia.org/wiki/Winsock). The code for `withSocketsDo` was approximately:

    withSocketsDo :: IO a -> IO a
    #if WINDOWS
    withSocketsDo act = do
        initWinsock
        act `finally` termWinsock
    #else
    withSocketsDo act = act
    #endif

Where `initWinsock` and `termWinsock` were C functions. Both checked a mutable variable so they only initialised/terminated once. The `initWinsock` function immediately initialised the Winsock library. The `termWinsock` function did not terminate the library, but merely installed an [`atexit`](http://pubs.opengroup.org/onlinepubs/009695399/functions/atexit.html) handler, providing a function that ran when the program shut down which terminated the Winsock library.

As a result, in all existing versions of the network library, it is fine to nest calls to `withSocketsDo`, call `withSocketsDo` multiple times, and to perform networking operations after `withSocketsDo` has returned.

**Future versions of network**

My approach to removing the requirement to call `withSocketsDo` was to make it very cheap, then sprinkle it everywhere it might be needed. Making such a function cheap on non-Windows just required an `INLINE` pragma (although its very likely GHC would have always inlined the function anyway).

For Windows, I changed to:

    withSocketsDo act = do evaluate withSocketsInit; act 

    {-# NOINLINE withSocketsInit #-}
    withSocketsInit = unsafePerformIO $ do
        initWinsock
        termWinsock

Now `withSocketsDo` is very cheap, with subsequent calls requiring no FFI calls, and thanks to [pointer tagging](http://community.haskell.org/~simonmar/papers/ptr-tagging.pdf), just a few cheap instructions. When placing additional `withSocketsDo` calls my strategy was to make sure I called it before constructing a `Socket` (which many functions take as an argument), and when taking one of the central locks required for the network library. In addition, I identified a few places not otherwise covered.

In newer versions of the `network` library it is probably never necessary to call `withSocketsDo` - if you find a place where one is necessary, let me know. However, for compatibility with older versions on Windows, it is good practice to always call `withSocketsDo`. Libraries making use of the network library should probably call `withSocketsDo` on their users behalf.
