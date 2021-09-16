# Reflecting on the Shake Build System

_Summary: As a medium-sized build system, Shake has some good bits and some bad bits._

I first developed the [Shake build system](https://shakebuild.com/) at Standard Chartered in 2008, [rewriting an open source version](https://shakebuild.com/faq#whats-the-history-of-shake) in my spare time in 2011. I wrote a paper on Shake for [ICFP 2012](https://ndmitchell.com/#shake_10_sep_2012) and then clarified some of the details in a [JFP 2020 paper](https://ndmitchell.com/#shake_21_apr_2020). Looking back, over a decade later, this post discusses what went well and what could be improved.

The first thing to note is that Shake is a medium sized build system. If you have either 1 source file or 1 million source files, Shake probably isn't a good fit. In this post I'm going to go through how Shake does as a medium-sized build system, and over the next two days subsequent posts will reflect on what I think a small or super-sized build system might look like.

The most important thing Shake got right was adding monadic/dynamic dependencies. Most build systems start with a static graph, and then, realising that can't express the real world, start hacking in an unprincipled manner. The resulting system becomes a bunch of special cases. Shake embraced dynamic dependencies. That makes some things harder (no static cycle detection, less obvious parallelism, must store dependency edges), but all those make Shake itself harder to write, while dynamic dependencies make Shake easier to use. I hope that eventually _all_ build systems gain dynamic dependencies.

In addition to dynamic dependencies, Shake has early cut-off, meaning files that rebuild but don't change don't invalidate rules that depend upon them. This feature is something increasingly becoming standard in build systems, which is great to see.

Shake is written as a Haskell DSL, which means users of Shake are writing a Haskell program that happens to heavily leverage the Shake library. That choice was a double-edged sword. There are some significant advantages:

* I didn't need to invent a special purpose language. That means I get to reuse existing tooling, existing learning materials, and existing libraries.
* Since Shake is just a library, it [can be documented](https://hackage.haskell.org/package/shake/docs/Development-Shake.html) with the normal tools like [Haddock](https://www.haskell.org/haddock/).
* Users can extend Shake using Haskell, and publish libraries building on top of Shake.
* The modelling of monadic dependencies in Haskell is pretty elegant, given a dedicated syntax for expressing monadic computations (the `do` keyword).
* Projects like [Haskell Language Server](https://github.com/haskell/haskell-language-server) can build on top of Shake in fairly fundamental ways. See our recent [IFL 2020 paper](https://ndmitchell.com/#hls_04_sep_2020) for the benefits that brought.

But there are also some downsides:

* Most significantly, the audience of Shake is somewhat limited by the fact that Shake users probably have to learn some Haskell. While the [user manual](https://shakebuild.com/manual) aims to teach enough Haskell to write Shake without really knowing Haskell, it's still a barrier.
* Haskell has some significant weaknesses, e.g. it has two competing package managers, countless distribution mechanisms, and none of these things are consistent for long periods of time. Haskell has a poor on-ramp, and thus so does Shake.

The choice of an embedded DSL for Shake also leads to the issue that Shake doesn't know when a rule has changed, since a rule is opaque Haskell code. As a consequence, if you modify a command line in a `.hs` file, Shake is unaware and won't rebuild the necessary files. There are a bunch of techniques for dealing with this limitation (see the Shake functions `shakeVersion`, `versioned`), but none are pleasant, and it remains an easy mistake to make. A potential way out is to build a system which reads configuration files not in Haskell and interprets them, which I [gave a talk about](https://ndmitchell.com/#shake_09_oct_2015), and I've seen deployed in practice. But it's something where each user ends up rolling their own.

Another limitation is that Shake is (deliberately) quite low-level. It gives you a way to depend on a file, and a way to run a command line. It doesn't give you a way to express a C++ library. The hope from the beginning was that Shake would be language neutral, and that libraries would arise that built on top of Shake providing access to standard libraries. If you were writing a Python/C++/Ruby build script, you'd simply import those libraries, mix them together, and have a working build system. There are libraries that have gone in that direction, the libraries [`shake-language-c`](https://hackage.haskell.org/package/shake-language-c) and [`shake-cpp`](https://github.com/jfeltz/shake-cpp) provide C++ rules, [`avr-shake`](https://hackage.haskell.org/package/avr-shake) lets you work with [AVR Crosspack](https://www.obdev.at/products/crosspack/index.html). Unfortunately, there aren't enough libraries to just plug together a build system. I think a fundamental problem is that it's not immediately obvious _how_ such libraries would compose, and without that composability, it's hard to build the libraries that would rely on composability.

Finally, the most surprising aspect about developing Shake is that a large part of the effort has gone into writing an ergonomic and cross-platform process executor. The result is found at [`Development.Shake.Command`](https://hackage.haskell.org/package/shake/docs/Development-Shake-Command.html), and can be used outside Shake, letting users write:

```haskell
cmd "gcc -c" [src]
```

This example invokes `gcc`, ensuring that `src` is properly escaped if it has spaces or other special characters. A significant amount of the engineering work in Shake has gone into that facility, when it's totally orthogonal to the build system itself.

In the next two parts of this series, I'll go through why I think Shake isn't a great build system for tiny projects (and what might be), followed by why Shake isn't great for absolutely huge projects (and what would need to be fixed).
