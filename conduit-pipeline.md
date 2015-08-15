# Parallel/Pipelined Conduit

_Summary: I wrote a Conduit combinator which makes the upstream and downstream run in parallel. It makes Hoogle database generation faster._

The Hoogle database generation parses lines one-by-one using [haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts), and then encodes each line and writes it to a file. Using Conduit, that ends up being roughly:

    parse =$= write

Conduit ensures that parsing and writing are interleaved, so each line is parsed and written before the next is parsed - ensuring minimal space usage. Recently the FP Complete guys [profiled Hoogle database generation](https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph#a-larger-example) and found each of these pieces takes roughly the same amount of time, and together are the bottleneck. Therefore, it seems likely that if we could parse the _next_ line while writing the _previous_ line we should be able to speed up database generation. I think of this as analogous to CPU pipelining, where the next instruction is decoded while the current one is executed.

I came up with the combinator:

     pipelineC :: Int -> Consumer o IO r -> Consumer o IO r

Allowing us to write:

     parse =$= pipelineC 10 write

Given a buffer size `10` (the maximum number of elements in memory simultaneously), and a `Consumer` (`write`), produce a new `Consumer` which is roughly the same but runs in parallel to its upstream (`parse`).

### The Result

When using 2 threads the Hoogle 5 database creation drops from 45s to 30s. The CPU usage during the pipelined stage hovers between 180% and 200%, suggesting the stages are quite well balanced (as the profile suggested). The parsing stage is currently a little slower than the writing, so a buffer of 10 is plenty - increasing the buffer makes no meaningful difference. The reason the drop in total time is only by 33% is that the non-pipelined steps (parsing Cabal files, writing summary information) take about 12s.

Note that Hoogle 5 remains unreleased, but can be tested from [the git repo](http://github.com/ndmitchell/hoogle) and will hopefully be ready soon.

### The Code

The idea is to run the `Consumer` on a separate thread, and on the main thread keep pulling elements (using `await`) and pass them to the other thread, without blocking the upstream `yield`. The only tricky bit is what to do with exceptions. If the consumer thread throws an exception we have to get that back to the main thread so it can be dealt with normally. Fortunately [async exceptions](http://neilmitchell.blogspot.co.uk/2015/05/handling-control-c-in-haskell.html) fit the bill perfectly. The full code is:

    pipelineC :: Int -> Consumer o IO r -> Consumer o IO r
    pipelineC buffer sink = do
        sem <- liftIO $ newQSem buffer  -- how many are in flow, to avoid excess memory
        chan <- liftIO newChan          -- the items in flow (type o)
        bar <- liftIO newBarrier        -- the result type (type r)
        me <- liftIO myThreadId
        liftIO $ flip forkFinally (either (throwTo me) (signalBarrier bar)) $ do
            runConduit $
                (whileM $ do
                    x <- liftIO $ readChan chan
                    liftIO $ signalQSem sem
                    whenJust x yield
                    return $ isJust x) =$=
                sink
        awaitForever $ \x -> liftIO $ do
            waitQSem sem
            writeChan chan $ Just x
        liftIO $ writeChan chan Nothing
        liftIO $ waitBarrier bar

We are using a channel `chan` to move elements from producer to consumer, a quantity semaphore `sem` to limit the number of items in the channel, and a barrier `bar` to store the return result (see [about the barrier type](http://neilmitchell.blogspot.co.uk/2012/06/flavours-of-mvar_04.html)). On the consumer thread we read from the channel and `yield` to the consumer. On the main thread we `awaitForever` and write to the channel. At the end we move the result back from the consumer thread to the main thread. The full implementation is [in the repo](https://github.com/ndmitchell/hoogle/blob/236cd2f786147361d57793f1b0e3301eafd2a107/src/General/Conduit.hs#L82).

### Enhancements

I have specialised `pipelineC` for `Consumer`s that run in the `IO` monad. Since the `Consumer` can do `IO`, and the order of that `IO` has changed, it isn't exactly equivalent - but relying on such `IO` timing seems to break the spirit of `Conduit` anyway. I suspect `pipelineC` is applicable in some other moands, but am not sure which (`ReaderT` and `ResourceT` seem plausible, `StateT` seems less likely).

_Acknowledgements: Thanks to Tom Ellis for helping figure out what type `pipelineC` should have._
