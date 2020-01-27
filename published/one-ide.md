# One Haskell IDE to rule them all

Summary: The Haskell IDE Engine and Ghcide teams are joining forces on a single IDE.

This weekend many of the [Haskell IDE Engine (HIE)](https://github.com/haskell/haskell-ide-engine) and [Ghcide](https://github.com/digital-asset/ghcide) developers met up for the [Bristol Hackathon](https://mpickering.github.io/bristol2020.html). Writing an IDE is a lot of work, and the number of contributors is finite, so combining forces has always seemed like a good idea. We now have a plan to combine our efforts, taking the best of each, and putting them together. Taking a look at the best features from each:

* HIE provides a lot of plugins which extend the IDE. A choice of three formatters. LiquidHaskell. HLint and Hoogle. There are lots, and they are quite mature.
* HIE has great build scripts that build the IDE for lots of different compilers and configurations.
* HIE has gained lots of arcane knowledge about LSP and various clients, with an understanding of how best to respond in terms of latency/liveness.
* HIE has driven a lot of improvements in the GHC API.
* HIE has pioneered a lot of code that Ghcide subsequently reused, e.g. completions and hover.
* Ghcide uses a Shake graph based approach to manage the IDE state, allowing a simpler programming model.
* Ghcide breaks the GHC monad apart, making it easier to do tricks like reusing `.hi` files and multi-component builds with a single GHC session.
* Both projects use the same set of underlying libraries - `haskell-lsp`, `lsp-test` and `hie-bios`.

Putting these together, we've decided that the best way forward is to create a new project at [`haskell/ide`](https://github.com/haskell/ide/) which combines the best of both. That project will be a source of plugins and a complete distribution of an IDE. Under the hood, it will use Ghcide as a library, making use of the core graph and logic. The new IDE project will take over plugins and build system from HIE. There are some things in Ghcide which will be ripped out into plugins, e.g. the code actions. There are some ideas in HIE that will be folded into Ghcide, e.g. liveness, plugin wrappers and LSP quirks. Together, we hope to create a world class IDE experience for Haskell.

In the short term, we don't recommend anyone switch to this new IDE, as we're still putting the pieces together - continue using whatever you were using before. If you're interested in helping out, we're tracking some of the major issues in [this ticket](https://github.com/haskell/ide/issues/1) and the IDE and Ghcide repos.

Thanks to everyone who has contributed their time to both projects! The current state is a consequence of everyone's time and open collaboration. The spirit of friendship and mutual assistance typifies what is best about the Haskell community.

By Alan Zimmerman, Neil Mitchell, Moritz Kiefer and everyone at the Bristol Hackathon
