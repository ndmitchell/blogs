# Fixing Haddock docs

_Summary: A few weeks ago Hackage stopped generating docs. I have a script that generates the docs, and also fixes some Haddock bugs._

A few weeks ago Hackage stopped generating documentation, so if you look at recently uploaded pages they tend to either lack docs, or have very alert maintainers who did a manual upload. I've packaged up my solution, which also fixes some pretty annoying Haddock bugs. Given that I can now get docs faster and better with my script, I'll probably keep using it even after Haddock on Hackage gets restarted.

**How to use it**

* You are likely to get better results if you always install the packages you use with documentation.
* Ensure you have `tar`, `curl`, `cp` and `cabal` on your `$PATH`.
* `cabal update && cabal install neil`
* Make a release, don't touch any other code, then make sure you are in the project directory.
* `neil docs --username=YourHackageUsername`
* Type in your Hackage password at the prompt.

And like that, your docs are online. To see an example of something that was generated with this process, look at [Shake](http://hackage.haskell.org/package/shake-0.13.4/docs/Development-Shake.html).

**What I fixed**

I automated the process using scripts originally taken from the [lens library](https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh), supplemented with suggestions from [Reddit](http://www.reddit.com/r/haskell/comments/2hxunn/upload_your_own_hackage_documentation/). I then do a number of manual fixups.

* Haddock now makes cross-module links where it doesn't know what the target is default to types. Concretely, if I write `'Development.Shake.need'` in Haddock it generates a link to `#t:need`, which doesn't exist, when it should be `#v:need` - I fix that.
* On Windows, if you use `CPP` and multiline bird-tick (`>`) Haddock blocks you get a blank line between each line. I fix that.
* If you follow some of the simpler scripts links outside your package won't work (or at least, didn't for me). I fix that.

**The `neil` tool**

The `neil` tool is my personal set of handy Haskell scripts. I make all my releases with it (`neil sdist`), and do lots of checks that my packages conform to my rules (`neil check`). I also use it for driving my [Travis](https://travis-ci.org/) instances. It's in fairly regular flux. Until now, I've always kept it in Darcs/Git and never released it - it's personal stuff for me. It's tuned for what I do, and I don't expect that to change.

You might also notice that `neil` provides a library. Don't use that, I intend to delete it in a few weeks.
