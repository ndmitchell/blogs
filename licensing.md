# Licensing my Haskell packages

_Summary: I plan to license all future packages under the "BSD-3-Clause OR Apache-2.0" license._

A few weeks ago I calculated that the distribution of Haskell library licenses is approximately:

* BSD-3-Clause: 67%
* MIT: 20%
* Apache-2.0: < 2%

In contrast, [Wikipedia](https://en.wikipedia.org/wiki/Apache_License) suggests for most open-source libraries the Apache-2.0 license beats BSD-3-Clause, and it is the permissive choice of FSF/Google etc. I was curious why it was so rare in the Haskell world, so [asked on Twitter](https://twitter.com/ndm_haskell/status/1027575192242085889). The resulting thread got my thinking about license choices, which changed my view of what license I'd like to use. In this post I'm going to go through my reasoning.

**The license I want to use**

What I _want_ to say is that anyone is free to use my code for any purpose. If they make changes to my code which would be of general benefit and have a chance of being accepted upstream, they should post those changes publicly. I give my work away freely, and I want the version I'm giving away freely to be the best possible version for everyone. No license matches this intent, none force you to share code that you improve but only use internally, and the legal definition of "general benefit" can only be arbitrated by me. As a result, I'd like people to follow those principles, but chose to give out my code with far less restrictions, in the hope people will do the right thing and share any improvements anyway.

**The license I use**

When I first started releasing code (around 2004) I originally licensed my code as GPL-2.0, because that was a restrictive open-source license and I was still dipping my toes in the open source pond. By 2007 I was [releasing new libraries](https://github.com/ndmitchell/uniplate/blame/555a97414769316db0110385b0780db50d2794e6/uniplate.cabal) as BSD-3-Clause, since that was what everyone in the Haskell community was using and seemed to provide the benefits I wanted (people sent me patches without being legally compelled to, just for the good of the code, which I prefer). It took until 2012 for me to switch my [earliest](https://github.com/ndmitchell/hlint/commit/cc455348189238d4d6bba700619037b91f61bd1d) [libraries](https://github.com/ndmitchell/hoogle/commit/d9b78531f5063985e42b0ff0fd746504e583e749) to BSD-3-Clause - one to avoid annoyance at work and another at the request of a commercial company who were linking it to proprietary pieces, and then went on to contribute extensively for the benefit of the project. Currently, all my projects are BSD3 licensed.

**The license I will use**

But what license _should_ I be using? These questions prompted me to hunt around and I came to the conclusion that the right license for me is:

    BSD-3-Clause OR Apache-2.0

Concretely, I plan to license all my code under _both_ the BSD-3-Clause and Apache-2.0 licenses, but a user is able to use it under either. My reasoning is as follows:

**Why BSD-3-Clause over MIT?**

I debated BSD-3-Clause vs MIT for a long time. They both offer substantially the same freedoms, but BSD-3-Clause also requires you can't use my name to endorse things you build - which seems reasonable. I like MIT because it's less ambiguous as a name, and it makes explicit freedoms that are implicit in the BSD-3-Clause license. The fact that BSD-3-Clause is more commonly used in Haskell and my existing code is a point in it's favour. In the end, I picked BSD-3-Clause.

**Why Apache-2.0?**

The Apache-2.0 license offers a patent grant - I'm promising that if you use my code I'm not going to sue you using my patents. If I gave you code and then later sued you for using it, that would be mean. More importantly (from my side at least) it ensures everyone contributing to my library is following the same basic "don't be mean" principle, so I can continue to use my code free from patent concerns.

**Why both?**

The Apache-2.0 license is incompatible with the GPL-2.0 and LGPL-2.1-only licenses, meaning any library building on my code plus the [GTK bindings](https://hackage.haskell.org/package/gtk) would be in a license quandary. By licensing under both most users can use Apache-2.0 (it gives you patent protection, so it's in your best interest), and those that would have problems otherwise can stick with BSD-3-Clause.

**Next steps**

Licensing is a slightly sensitive topic, so I'm declaring my intent, and waiting for feedback. Hopefully this change is positive for everyone, but anyone with concerns should let me know. As to technical detail, Cabal 2.2 [supports SPDX license expressions](http://hackage.haskell.org/package/Cabal-2.2.0.1/docs/Distribution-SPDX-LicenseExpression.html), which is the syntax I've been using throughout this post.
