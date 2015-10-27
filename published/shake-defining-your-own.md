# Defining your own Shake build system

_Summary: The video of my recent Shake talk is now online._

My [Haskell eXchange 2015](https://skillsmatter.com/conferences/7069-haskell-exchange-2015) talk "Defining your own build system with Shake" is now online, with both [slides](file:///C:/Neil/ndmitchell.github.io/downloads/slides-defining_your_own_build_system_with_shake-09_oct_2015.pdf) and [video](https://skillsmatter.com/skillscasts/6548-defining-your-own-build-system-with-shake). As always, all my talks and papers are available from [ndmitchell.com](http://ndmitchell.com/).

This Shake talk took a distinct approach from my previous talks - partly because I now have a better understanding of how large Shake build systems are typically structured. The key theoretical innovation of Shake is [monadic dependencies](http://neilmitchell.blogspot.co.uk/2014/07/applicative-vs-monadic-build-systems.html), and the key technical innovation is using Haskell as an embedded language. Putting these pieces together lets Shake users write a build system _interpreter_, rather than a monolithic build system.

In the talk I suggest that build systems can be split into metadata (changes frequently, can be tracked, custom format for each project relying on conventions) and an interpreter for that metadata (written using Shake). The result seems to work quite nicely, especially in larger projects where everyone is expected to update the metadata, but changes to the interpreter are expected to be rarer and more carefully thought out.

One of the goals of the talk was to convert some of the audience to using Shake. It seems to have worked, as [`@krisajenkins`](https://twitter.com/krisajenkins) [tweeted](https://twitter.com/krisajenkins/status/653571446774624256):

> First fruit of #haskellx - switched my makefiles over to Shake. Parallelised builds & a readable syntax. Much win.

