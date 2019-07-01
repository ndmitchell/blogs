# Thoughts for a Haskell IDE

_Summary: We have been working on pieces for a Haskell IDE at Digital Asset._

At [Digital Asset](https://digitalasset.com), we wrote the [DAML programming language](https://daml.com). The compiler builds on GHC, and one of the important tools for using DAML is an IDE. You can try the [DAML IDE online](https://webide.daml.com) or [download it](https://daml.com). Since we wrote the DAML IDE in Haskell, and DAML uses GHC under the hood, it's possible to take the work we did for the DAML IDE and turn them into pieces for a Haskell IDE. In the rest of this post I'll outline what we wrote, and how I think it can make a full Haskell IDE.

**What has Digital Asset written?**

We have written a [Haskell library `hie-core`](https://github.com/digital-asset/daml/tree/master/compiler/hie-core), which serves as the "core" of an IDE. It maintains state about which files are open. It generates diagnostics. It runs the parser and type checker. It doesn't figure out how to load your package, and it doesn't have integrations with things like HLint etc. In my view, it should never gain such features - it's deliberately a small core of an IDE, which can be extended with additional rules and handlers after-the-fact.

On the technical side, at the heart of the IDE is a key-value store, where keys are pairs of file names and stages (e.g. `TypeCheck`) and values are dependent on the stage. We use [the Shake build system](https://shakebuild.com) in [memory-only mode](https://neilmitchell.blogspot.com/2018/10/announcing-shake-017.html) to record dependencies between phases. As an example of a rule:

```
define $ \TypeCheck file -> do
    pm <- use_ GetParsedModule file
    deps <- use_ GetDependencies file
    tms <- uses_ TypeCheck (transitiveModuleDeps deps)
    packageState <- use_ GhcSession ""
    opt <- getIdeOptions
    liftIO $ Compile.typecheckModule opt packageState tms pm
```

To type check a file, we get the parse tree, the transitive dependencies, a GHC session, and then call a `typecheckModule` helper function. If any of these dependencies change (e.g. the source file changes) the relevant pieces will be rerun.

Building on top of Shake wasn't our first choice - we initially explored two painful dead ends. While Shake isn't perfect for what we want, it's about 90% of the way there, and having robust parallelism and many years of solid engineering is worth some minor compromises in a few places. Having all the features of Shake available has also been exceptionally helpful, allowing us to try out new things quickly.

**What else is required for an IDE?**

My hope is that `hie-core` can become the core of a future IDE - but what else is required?

* Something to load up a GHC session with the right packages and dependencies in scope. For DAML, we have a custom controlled environment so it's very easy, but real Haskell needs a better solution. My hope is that [`hie-bios`](https://github.com/mpickering/hie-bios) becomes the solution, since I think it has a great underlying design.
* Some plugins to add features, such as the as-yet-unwritten [`hie-hlint`](https://github.com/ndmitchell/hlint) and [`hie-ormolu`](https://github.com/tweag/ormolu). Since we add lots of features on top of `hie-core` to make the DAML IDE, we have a good story for extensions in `hie-core`. Importantly, because `shake` is designed to be extensible, these extensions can integrate with the full dependency graph.
* Something to talk  [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) to communicate with editors, for which we use the existing [`haskell-lsp`](https://github.com/alanz/haskell-lsp).
* An extension for your editor. We provide a [VS Code extension](https://code.visualstudio.com/api) as [`extension` in `hie-core`](https://github.com/digital-asset/daml/tree/master/compiler/hie-core/extension), but it's a fairly boilerplate LSP implementation, and people have got it working for Emacs already.
* Something to put it all together into a coherent project, generate it, distribute it etc. A project such as [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine) might be the perfect place to bring everything together.

**Can I try it now?**

Yes - [instructions here](https://github.com/digital-asset/daml/tree/master/compiler/hie-core#readme). I've been using `hie-core` as my primary Haskell development environment since [ZuriHac](https://zfoh.ch/zurihac2019/) two weeks ago, and I like it a lot. However, beware:

* The IDE doesn't load all the relevant files, only the ones you have open.
* Integration with things like `stack` doesn't work very well - I've been using `hie-bios` in "Direct" mode - giving it the flags to start `ghci` myself. See my integrations for [`shake`](https://github.com/ndmitchell/shake/blob/master/hie.yaml) and [`hlint`](https://github.com/ndmitchell/hlint/blob/master/hie.yaml).
* Features like `hs-boot` files and Template Haskell need more work to be fully supported, although a bit of Template Haskell has been observed to work.

These issues are being discussed [on the `hie-bios` issue tracker](https://github.com/mpickering/hie-bios/issues/9).

**Hypothetical FAQ**

_Q: Is something like FRP better than Shake for describing dependencies?_ A: I think it's clear that an IDE should use some dependency/incremental computation/parallel rebuilding approach. Shake offers one of those, and is well tested, exception safe, performant etc. The mapping from Shake to what we really want is confined to a [single module](https://github.com/digital-asset/daml/blob/master/compiler/hie-core/src/Development/IDE/Core/Shake.hs), so feel free to experiment with alternatives.

_Q: Who has contributed?_ Many many people have contributed pieces, including the whole team at Digital Asset, in particular [Tim Williams](https://github.com/willtim), [David Millar-Durant](https://github.com/DavidM-D/), [Neil Mitchell](https://ndmitchell.com/) and [Moritz Kiefer](https://github.com/cocreature).

_Q: What is the relationship to haskell-ide-engine?_ My hope is this piece can slot into the other great things that have been done to make IDE tooling better, specifically [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine). This post is intended to start that discussion.
