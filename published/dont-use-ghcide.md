# Don't use Ghcide anymore (directly)

_Summary: I recommend people use the Haskell Language Server IDE._

Just over a year ago, I recommended people looking for a Haskell IDE experience to [give Ghcide a try](https://ndmitchell.com/#ghcide_07_sep_2019). A few months later the Haskell IDE Engine and Ghcide teams [agreed to work together](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html) on [Haskell Language Server](https://github.com/haskell/haskell-language-server) - using [Ghcide as a library](https://github.com/haskell/ghcide) as the core, with the plugins/installer experience from the [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine) (by that stage we were already both using the same [Haskell setup](https://github.com/mpickering/hie-bios) and [LSP](https://github.com/alanz/haskell-lsp) libraries). At that time [Alan Zimmerman](https://github.com/alanz) said to me:

> "We will have succeeded in joining forces when you (Neil) start recommending people use Haskell Language Server."

I'm delighted to say that time has come. For the last few months I've been both using and recommending [Haskell Language Server](https://github.com/haskell/haskell-language-server) for all Haskell IDE users. Moreover, for VS Code users, I recommend simply installing the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) which downloads the right version automatically. The experience of Haskell Language Server is better than either the Haskell IDE Engine or Ghcide individually, and is improving rapidly. The teams have merged seamlessly, and can now be regarded as a single team, producing one IDE experience.

There's still [lots of work to be done](https://github.com/haskell/haskell-language-server/issues). And for those people developing the IDE, Ghcide remains an important part of the puzzle - but it's now a developer-orientated piece rather than a user-orientated piece. Users should follow the [README at Haskell Language Server](https://github.com/haskell/haskell-language-server#readme) and report bugs [against Haskell Language Server](https://github.com/haskell/haskell-language-server/issues).
