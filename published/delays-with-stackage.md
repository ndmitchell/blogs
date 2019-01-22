# Release delays with Stackage

_Summary: There are two steps that delay new versions of packages in Stackage._

I aim to get the latest version of my software out to as many people as quickly as possible. Older versions have bugs, new versions have new features - that's why I release new versions. Unfortunately there are two steps in [Stackage](https://github.com/commercialhaskell/stackage) that slow down this process.

Taking an example, [HLint](https://github.com/ndmitchell/hlint#readme) depends on [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts), and is tightly coupled, so (to a first approximation) every 0.1 bump to `haskell-src-exts` requires changing HLint. There are also lots of other packages that depend on `haskell-src-exts`. This situation leads to two delays in getting HLint to Stackage users, both of which are on display in [bug 4214](https://github.com/commercialhaskell/stackage/issues/4214):

**Issue 1: Reluctance to remove packages**

Stackage has a policy that if a new package (e.g. `haskell-src-exts`) is released which breaks your package (e.g. `haskell-src-meta`) you have [an unspecified amount of time](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md#following-dependency-upgrades) to release an update. My experience is either packages are updated quickly (all upgrades on that ticket happened within 12 days) or the package maintainers never reply (46 days later no other maintainer has even left a comment).

It used to be the case that there were hard time limits (maximum one month), but my experience was those were never enforced. Unfortunately this lag can cause a significant delay until Stackage Nightly picks up an upgrade. It seems like a more mechanical rule (e.g. after 2 weeks with no update, or 6 weeks total) might keep the process ticking faster. I appreciate it's hard to break people's work, which is why making it come down to human judgement seems to lengthen the process significantly.

_Delay imposed: up to 2 months, and sometimes requires chasing._

**Issue 2: Existence of Stackage LTS**

While the first issue is very much a trade off, the second one is (in my view) just a bad design of Stackage, as [I've said before](https://neilmitchell.blogspot.com/2015/12/whats-point-of-stackage-lts.html). There is Stackage Nightly which has the latest code. There is Stackage LTS which has older and therefore buggier code, up to 2-3 months older. Having two options is fine, but the `stack` tool and documentation direct people towards LTS as a preference. LTS is useful if you view the act of upgrading between 0.0.1 versions as low risk (which it isn't) or you find it easier to fix multiple distinct breaking changes when they are overlapped (which it isn't). Unfortunately Stackage LTS users won't get a new version of HLint until a new Stackage LTS version is created, even after it gets merged. On the plus side, this process happens automatically without any intervention by package authors.

_Delay imposed: 2-3 months._

PS. While I criticise Stackage, that's because I want to make it better, since it is a very useful distribution channel for many people, and I'm grateful for the work the Stackage maintainers do to keep the process ticking along.
