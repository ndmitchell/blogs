# GHCid 0.6 Released

_Summary: I've released a new version of GHCid, which can interrupt running tests._

I've just released version [0.6.1 of GHCid](https://hackage.haskell.org/packages/ghcid). To a first approximation, `ghcid` opens `ghci` and runs `:reload` whenever your source code changes, formatting the output to fit a fixed height console. Unlike other Haskell development tools, `ghcid` is intended to be _incredibly robust_ - it works when nothing else does. This new version features:

**Much faster:** GHCid now passes `-fno-code` to `ghci` when it makes sense, which is about twice as fast.

**Interruptible test commands:** Since version 0.4 `ghcid` has supported a `--test` flag to pass a test command (e.g. `:main`) which is run whenever the code is warning free. As of version 0.6 that command will be interrupted if it needs to `:reload`, allowing long running tests and persistent "tests" - e.g. spawning web servers. Thanks to [Alex]() for showing it was possible as part of his `ordeal` project and [Luigy]() for merging that with GHCid. 

**Stack integration:** If you have a `stack.yaml` function and a `.stack-work` directory it will use [`stack` commands]() to run your project. Thanks to the Stack Team, in particular [Michael Sloan](), for helping get through all the hoops and providing the necessary functionality in Stack.

**More restart/reload flags:** It's been possible for a while to pass `--restart` to restart `ghci` if certain files change (e.g. the `.cabal` file). Now there is a separate `--reload` flag to cause `:reload` instead of a full restart, and both flags can take directories instead of individual files. 

**Major relayering:** For 0.6 I significantly refactored much of the GHCid code. There has always been an underlying [`Language.Haskell.Ghcid`]() API, and GHCid was built on top. With the new version the underlying library has been given a significant refactoring, mostly so that interruptions are handled properly without race conditions and with a sane multithreading story. On top of that is a new [`Session`]() layer, which provides a session abstraction - a `ghci` instance which tracks more state (e.g. which warnings have been emitted for already loaded files). Then the [`Ghcid`]() module builds on top, with much less state management. By simplifying and clarifying the responsibility of each layer certain issues such as leaking old `ghci` processes and obscure race conditions disappeared.

------------------------

I've been making use of many of these features in the [Shake website generator](), which I invoke with:

    ghcid --test=":main debug" --reload=parts --reload=../docs

This project uses Stack, so relies on the new `stack` integration. It runs `:main debug` as the test suite, which generates the website whenever the code reloads. Furthermore, if any of the `parts` (template files) or `docs` (Markdown pages) change the website regenerates. I can now edit the website, and saving it automatically regenerates the web pages within a second.
