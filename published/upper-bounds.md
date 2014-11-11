# Upper bounds or not?

_Summary: I thought through the issue of upper bounds on Haskell package dependencies, and it turns out I don't agree with anyone :-)_

There is currently a debate about whether Haskell packages should have upper bounds for their dependencies or not. Concretely, given `mypackage` and `dependency-1.0.2`, should I write `dependency >= 1` (no upper bounds) or `dependency >= 1 && < 1.1` ([PVP/Package versioning policy](https://www.haskell.org/haskellwiki/Package_versioning_policy) upper bounds). I came to the conclusion that the bounds should be `dependency >= 1`, but that [Hackage](https://hackage.haskell.org/) should automatically add an upper bound of `dependency <= 1.0.2`.

**Rock vs Hard Place**

The reason the debate has continued so long is because both choices are unpleasant:

* Don't add upper bounds, and have packages break for your users because they are no longer compatible.
* Add PVP upper bounds, and have reasonable install plans rejected and users needlessly downgraded to old versions of packages. If one package requires a minimum version of above _n_, and another requires a maximum below _n_, they can't be combined. The PVP allows adding new functions, so even if all your dependencies follow the PVP, the code might still fail to compile.

I believe there are two relevant relevant factors in choosing which scheme to follow.

_Factor 1: How long will it take to update the `.cabal` file_

Let us assume that the `.cabal` file can be updated in minutes. If there are excessively restrictive bounds for a few minutes it doesn't matter - the code will be out of date, but only by a few minutes, and other packages requiring the latest version are unlikely.

As the `.cabal` file takes longer to update, the problems with restrictive bounds become worse. For abandoned projects, the restrictive upper bounds make them unusable. For actively maintained projects with many dependencies, bounds bumps can be required weekly, and a two week vacation can break actively maintained code.

_Factor 2: How likely is the dependency upgrade to break_

If upgrading a dependency breaks the package, then upper bounds are a good idea. In general it is impossible to predict whether a dependency upgrade will break a package or not, but my experience is that most packages usually work fine. For some projects, there are stated compatibility ranges, e.g. [Snap](https://hackage.haskell.org/package/snap) declares that any API will be supported for two 0.1 releases. For other projects, some dependencies are so tightly-coupled that every 0.1 increment will almost certainly fail to compile, e.g. the [HLint](https://hackage.haskell.org/package/hlint) dependency on [Haskell-src-exts](https://hackage.haskell.org/package/haskell-src-exts). 

The fact that these two variable factors are used to arrive at a binary decision is likely the reason the Haskell community has yet to reach a conclusion.

**My Answer**

My current preference is to normally omit upper bounds. I do that because:

* For projects I usually heavily, e.g. haskell-src-exts, I have fairly regular communication with the maintainers, so am not surprised by releases.
* For most projects I depend on only a fraction of the API, e.g. [wai](https://hackage.haskell.org/package/wai), and most changes are irrelevant to me.
* Michael Snoyman and the excellent [Stackage](http://www.stackage.org/) alert me to broken upgrades quickly, so I can respond when things go wrong.
* I maintain quite a few projects, and the administrative overhead of uploading new versions, testing, waiting for continuous-integration results etc would cut down on real coding time. (While the Hackage facility to [edit the metadata](https://github.com/haskell/hackage-server/issues/52) would be quicker, I think that tweaking fundamentals of my package, but skipping the revision control and continuous integration, seems misguided.)
* The PVP is a heuristic, but usually the upper bound is too tight, and occasionally the upper bound is too loose. Relying on the PVP to provide bounds is no silver bullet.

On the negative side, occasionally my packages no longer compile for my users (very rarely, and for short periods of time, but it has happened). Of course, I don't like that at all, so do include upper bounds for things like haskell-src-exts.

**The Right Answer**

I want my packages to use versions of dependencies such that:

* All the features I require are present.
* There are no future changes that stop my code from compiling or passing its test suite.

I can achieve the first objective by specifying a lower bound, which I do. There is no way to predict the future, so no way I can restrict the upper bound perfectly in advance. The right answer must involve:

* On every dependency upgrade, Hackage (or some agent of Hackage) must try to compile and test my package. Many Haskell packages are already tested using [Travis CI](https://travis-ci.org/), so reusing those tests seems a good way to gauge success.
* If the compile and tests pass, then the bounds can be increased to the version just tested.
* If the compile or tests fail, then the bounds must be tightened to exclude the new version, and the author needs to be informed.

With this infrastructure, the time a dependency is too tight is small, and the chance of breakage is unknown, meaning that Hackage packages should have exact upper bounds - much tighter than PVP upper bounds.

_Caveats:_ I am unsure whether such regularly changing metadata should be incorporated into the `.cabal` file or not. I realise the above setup requires quite a lot of Hackage infrastructure, but will buy anyone who sorts it out some beer.
