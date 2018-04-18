# Ghcid with colors

_Summary: I've just released ghcid-0.7, which provides a much better user experience._

[Ghcid](https://github.com/ndmitchell/ghcid) is now over three years old, with [28 versions](https://github.com/ndmitchell/ghcid/blob/master/CHANGES.txt), but I'm particularly pleased with the improvements in the latest version. The focus has been on better defaults and a more polished user experience, some things you might spot:

*Color output:* GHC 8.2 added colored output, with important information highlighted. Previously Ghcid would explicitly disable that color. Now Ghcid embraces that color, turning the flag on for GHC versions that support it and ensuring any output munging is aware of the colors. It also enables colors in [Hspec](https://hackage.haskell.org/package/hspec) and colors the "All good" message green.

*Color defaults:* While enabling more color, it also provides `--color=never` to disable colors, and auto-detects when colors are likely to work well.

*Error spans:* Ghcid has always recommended that people turn on the `-ferror-spans` flag, but now it does it for you. For people using the [VS Code addin](https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid) that will provide a smoother experience out of the box.

*Parallel compilation:* Ghcid now passes `-j` to `ghci`, which I find speeds up compilation by about one third. Not a huge speedup, but still useful.

*Tracking files:* Ghcid now tracks both the `.ghcid` file (which you can use to specify the command line you want to use with `ghcid`) and `.ghci` file (which configures `ghci`). If either change it will cause Ghcid to restart, picking up the changes.

*Absolute paths:* The internals of Ghcid have been rewritten to always use absolute file paths, rather than relative paths. If your `ghci` wrapper changes directory (as I believe multi-project `cabal new-repl` does) Ghcid will continue to work.

*Enabling IDE improvements:* I have improved the integration features for editor plugins - you can now output a `.json` file with the parsed messages, including start/end position, and escape codes. There is a new `--setup` flag for sending initial messages to the underlying `ghci`. I haven't modified any of the IDE plugins to take advantage of these new features, but that's phase 2.

*Ctrl-C and cleaning up processes:* Ghcid is a continual fight to deal properly with Ctrl-C and close down all appropriate processes at the right time. In this release I've fought the battle in a few more corners, seemingly with some level of success.

*Crazy extensions:* GHC 8.4 is now able to deal with both `RebindableSyntax` and `OverloadedStrings` and still start `ghci`. I've modified Ghcid so it can also deal with this composition.

Together these changes make for a much more pleasant user experience.
