# Hoogle 5 is coming

_Summary: I'm working on Hoogle 5. If you like unfinished software, you can [try it](http://hoogle.haskell.org/)._

For the last month I've been working on the next version of Hoogle, version 5. It isn't finished, and it isn't the "official" version of Hoogle yet, but it's online, you can try it out, and anyone who wants to hack on Hoogle (or things like [hoogle-index](https://github.com/bgamari/hoogle-index)) should probably take a look now.

**How do I try it?**

The purpose of this blog post isn't to solicit beta testers, it's not ready for that - I'm mostly reaching out to Hoogle _developers_. That said, I know some people will want to try it, so it's online at [hoogle.haskell.org](http://hoogle.haskell.org/). Beware, it isn't finished, and [haskell.org/hoogle](http://haskell.org/hoogle) remains the "official" version to use (but hey, use whichever you want, or both, or neither).
 
**What's new for users of the website?**

* It isn't finished. In particular, type search hasn't been implemented. But there are also lots of other pieces in all the corners that don't work properly.
* It searches all the packages on [Stackage](http://www.stackage.org/) by default.
* There is a drop-down to allow you to restrict focus to only a single package, or an author, or a tag.

**What's new for everyone else?**

* There is no library, it's not on Hackage and the command line tool isn't designed for users. These things will be coming over time.
* It's hosted on a haskell.org virtual machine, served through [Warp](https://hackage.haskell.org/package/warp) rather than as a CGI program. Thanks to the [Haskell Infrastructure team](https://www.haskell.org/haskellwiki/Haskell.org_infrastructure) for all their help. As a result, I'm free to experiment with Hoogle without accidentally making the Haskell homepage say "moo".
* Generating database for all of Stackage (minus download time) takes about 40s and uses < 1Gb of memory. The databases are built directly out of the .tar.gz files, without unpacking them.

**What's next?**

Hoogle 5 is a complete rewrite of Hoogle, stealing bits as they were useful. At the moment Hoogle 4 is hosted at [https://github.com/ndmitchell/hoogle](https://github.com/ndmitchell/hoogle) while version 5 is at [https://github.com/ndmitchell/hogle](https://github.com/ndmitchell/hogle). The next step is to rename and move github repos so they are sensible once again. My best guess is that I should rename `hoogle` to `hoogle4`, then rename `hogle` to `hoogle` and move the issue tickets. I'm open to other suggestions.

Once that's resolved, I need to continue fixing and improving Hoogle so all the things that aren't finished become finished. If you are interested in helping, I recommend you create a github ticket describing what you want to do and we can take it from there.
