# How to Rewrite the Prelude

_Summary: Rewriting the Prelude should be done in a separate package and take a few years to complete, hopefully building a consensus and improving the result._

My [recent suggestion](http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html) that the Prelude shouldn't be rewritten to incorporate `Foldable`/`Traversable` generated [a lot of feedback](http://www.reddit.com/r/haskell/comments/2hzqii/neil_mitchells_haskell_blog_why/). It's clear some people strongly agree, and some strongly disagree. So instead of continuing the argument, in this post I'm going to describe how I think the people who want to change the Prelude should go about it.

There were a lot of comments that we should optimise Haskell for practitioners, not beginners - in particular that there should be `Prelude` (for advanced users) and `Prelude.Simple` (for beginners). My personal opinion is that if we have two Prelude's, why not make the beginner one the default one? Switching to the non-default one probably takes one line of moderately advanced syntax (currently `import Prelude(); import Prelude.Advanced`). For a beginner, writing a simple one line program, that more than doubles the complexity. For a moderate user, that's almost certainly outweighed by lines of LANGUAGE pragmas. (I'm also not sure that _I_ want the "Advanced" Prelude, but that's not relevant to this post.)

**Step 1: Write the new Prelude in a package**

The first step to proposing a new Prelude should be to write `Prelude.Advanced` in a separate package on Hackage. Let's assume we have a package `base-advanced` with a module `Prelude.Advanced`. The Prelude is not trivial, and translating high-level design goals (e.g. move `Foldable` into the Prelude) requires some design choices to translate into concrete code.

**Step 2: Gain users and feedback**

Rather than roll out the Prelude to everyone at once, slowly try and persuade people that your Prelude is superior to the existing one. In doing so, advanced users can start trying out the Prelude, and describing their experiences with it. Does it require `Foldable` to gain a `size` method? Does it show up a GHC error message that could be better? Do we need some new type of defaulting? Does profiling give poor results without some manually inserted cost centre? Is a direct list isomorphism a better type class to start from (as F# does with IEnumerable)? Given the number of enthusiastic supporters, this step should be easy.

**Step 3: Incorporate feedback**

The feedback from step 2 needs incorporating into the package, and potentially into GHC itself. I expect that will result in a significant number of revisions, and perhaps significant departures from the original design. Note that the [classy-prelude](https://hackage.haskell.org/package/classy-prelude) package is following the steps above, and has had 38 revisions on Hackage so far.

**Step 4: Gain acceptance**

Before going any further, some critical mass of people need to agree the new way is preferable. There may turn out to be competing proposals, and hopefully as a community we can find consensus and identify the best ideas (as we are currently doing with IO streaming libraries).

**Step 5: Move to base**

Once we have agreement on what the new Prelude should look like, it should probably be moved into base. At this point, based on all the experience we've got, we can decide whether it becomes `Prelude` or `Prelude.Advanced`. At the same time as moving into base, we should decide on the simpler story, but what that decision looks like, will depend on what the proposed Prelude looks like.

**The current approach**

There are a number of areas that [current approach](https://ghc.haskell.org/trac/ghc/ticket/9586) worry me. The change was made directly in the base Prelude, based on some agreement of high-level design decisions, and has already resulted in unexpected additions to the Foldable class. The Prelude will first be exposed to lots of users once GHC 7.10 ships, by which point iterating on feedback will be slow and difficult. The plan for the "beginner" version is to encourage beginners to use the haskell2010 package, which I don't think has been thought out (as someone who tried to use the haskell98 package for a short while, the exception change meant you couldn't use haskell98 and base in the same package, which is basically fatal).

The Haskell community changed `Applicative` to be a superclass of `Monad`. That change was simple and thoroughly thought out over a period of years. Warnings were added to GHC, packages were analysed, people experimented with what it would mean, and once decided the change took a number of GHC releases to fully roll out. I consider this change to have been done the right way. In contrast, the complete rewrite of the Prelude is happening far more quickly, and I would argue _too_ quickly. By taking a bit more time hopefully we can come up with something even better.

