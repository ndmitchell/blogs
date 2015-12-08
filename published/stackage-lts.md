# What's the point of Stackage LTS?

_Summary: Stackage LTS is mostly pointless._

[Stackage](https://www.stackage.org/) provides a set of precise versions of a subset of [Hackage](https://hackage.haskell.org/) which all place nicely together. At any one time, there are two such version sets:

* Nightly, which aims to be the latest version of all packages.
* LTS (Long Term Support), which with every major release updates every package, and every minor release updates packages that only changed their minor version.

[Stack](https://github.com/commercialhaskell/stack) currently defaults to LTS. I don't think LTS fulfils any need, and does cause harm (albeit very mild harm), so should be abandoned. I share these somewhat half-baked thoughts to hopefully guide how I think Stackage should evolve, and there may be good counterarguments I haven't thought of.

#### Why is Nightly better?

Nightly always has newer versions of packages. I believe that newer versions of packages are better.

* As a package author, with each new release, I fix bugs and evolve the library in ways which I think are beneficial. I make mistakes, and new versions are how I fix them, so people using old versions of my software are suffering for my past mistakes.
* As a package user, if I find a bug, I will endeavour to report it to the author. If I'm not using the latest version, it's highly likely the author will ask me to upgrade first. If a feature I want gets added, it's going to be to the latest version. The latest version of a package always has better support.
* If the latest version of a package does not suit my needs, that's an important alarm bell, and I need to take action. In some cases, that requires finding a fork or alternative package to do the same thing. In some cases, that involves talking to the author to make them aware of my particular use case. In either case, doing this sooner rather than later makes it easier to find a good solution.

#### What's the benefit of LTS?

As far as I am aware, LTS major releases are just Nightly at a particular point in time - so if you only ever use x.0 LTS, you might as well just use Nightly. The main benefit of LTS comes from picking a major LTS version, then upgrading to the subsequent minor LTS versions, to access new minor versions of your dependencies.

Upgrading packages is always a risk, as [LTS Haskell says](https://github.com/fpco/lts-haskell#possible-breakage). What LTS minor releases do are minimize the risk of having to fix compilation errors, at the cost of not upgrading to the latest packages. However, the reasons I do upgrades are:

* Sometimes, if one package has a known bug that impacts me, I specifically upgrade just that one package. I don't want to upgrade other packages (it introduces unnecessary risk), so I take my package set (be it LTS or Nightly) and replace one constraint.
* Every so often, I want to upgrade all my packages, so that I'm not missing out on improvements and so I'm not falling behind the latest versions - reducing the time to upgrade in future. I do that by picking the latest version of all packages, fixing breakages, and upgrading.
* When upgrading, compile-time errors are a minor issue at worst. Before Stackage, my major headache was finding a compatible set of versions, but now that is trivial. Fixing compile-time errors is usually a little work, but fairly easy and predictable. Checking for regressions is more time consuming, and running the risk of regressions that escape the test suite has a significant cost. Tracking down if there are any resulting regressions is very time-consuming.

I don't see a use case for upgrading "a little bit", since I get a lot less benefit for only marginally less work.

#### Why is LTS better?

When I asked [this question](https://twitter.com/ndm_haskell/status/660073990602547200?lang=en-gb) on Twitter, I got two reasons that did seem plausible:

* [Matt Parsons](https://twitter.com/mattoflambda) suggested that LTS provided a higher likelihood of precompiled binaries shared between projects. That does make sense, but a similar benefit could be achieved by preferring Nightly on the 1st on the month.
* [Gabriel Gonzalez](https://twitter.com/GabrielG439) suggested that when including a resolver in tutorials, LTS looks much better than a Nightly. I agree, but if there only was Nightly, then it wouldn't look quite as bad.

If Haskell packages had bug fixes that were regularly backported to minor releases, then the case for an LTS version would be stronger. However, my experience is that fixes are rarely backported, even for the rare security issues.

#### Is this a problem?

Currently everyone developing Haskell has two choices, and thanks to Stack, might not even be aware which one they've ended up making (Stack will pick nightly on its own if it thinks it needs to). Reducing the choices and  simplifying the story removes work for the Stackage maintainers, and cognitive burden from the Stackage users.

Stackage was a radical experiment in doing things in an entirely different way. I think it's fair to say it has succeeded. Now, after experience using the features, I think it's right to critically look at tweaks to the model.
