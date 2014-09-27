# GHCID - a new GHCi based IDE (ish)

_Summary: I've just released `ghcid`, which interactively shows the errors in your project on each save._

I'm please to announce [`ghcid`](https://github.com/ndmitchell/ghcid), which is either "GHCi as a daemon" or "GHC + a bit of an IDE". I've been using it for my development for the last few days, and already find it quite valuable. Unlike other Haskell development tools, `ghcid` is intended to be _incredibly simple_. In particular, it doesn't integrate with any editors, doesn't depend on GHC the library and doesn't start web servers. It's under 200 lines of fairly dull Haskell, which talks over pipes to your existing `ghci`.

**Using it**

Run `cabal update && cabal install ghcid` to install it as normal. Then run `ghcid --height=10 "--command=ghci Main.hs"`. The `height` is the number of lines you are going to resize your console window to (defaults to 8), and the `command` is how you start this project in `ghci`. Personally, I always create a `.ghci` file at the root of all my projects, which usually reads [something like](https://github.com/ndmitchell/ghcid/blob/master/.ghci):

    :set -fwarn-unused-binds -fwarn-unused-imports
    :set -isrc
    :load Main

With that you can pass `--command=ghci` (or nothing, since that is the default).

After that, resize your console and make it so you can see it while working in your editor. On Windows the `ghcid` console will automatically sit on top of all other windows. On Linux, you probably want to use your window manager to make it topmost or use a [tiling window manager](http://xmonad.org/).

**What you get**

On every save you'll see a list of the errors and warnings in your project. It uses a single `ghci` under the hood, so even relatively large projects should update their status pretty quickly. As an example:

    Main.hs:23:10:
        Not in scope: `verbosit'
        Perhaps you meant `verbosity' (imported from System.Console.CmdArgs)
    Util.hs:18:1: Warning: Defined but not used: `foo'

Or, if everything is good, you see:

    All good

This project is only a few days old, so please [report any bugs](https://github.com/ndmitchell/ghcid/issues) you find.

**What you want**

I regularly use an IDE to develop in a Haskell-like language. I find that with the IDE I'm about 25% more productive than without it. While an IDE can provide lots of neat features (go to definition, search, type tooltips) I think most of the productivity gains come from:

1. Syntax coloring.
2. A list of errors and warnings...
3. ...which is updated as you type...
4. ...and are highlighted in the text (red squiggles).

Every text editor already provides syntax coloring. With `ghcid` you get the list of errors and warnings. To get the final two features you need to integrate with an editor. I'm hoping that `ghcid` can do some of the heavy lifting by taking a directory of files to treat as overrides, and producing a list of warnings/errors to a file. The rest is editor specific, and I hope to attempt integration with [Sublime Text](http://www.sublimetext.com/) at some point (although would love some help).
