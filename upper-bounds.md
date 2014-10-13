# Upper bounds or not?

There is currently a ferocious debate going on about whether packages should have upper bounds for their dependencies or not. Concretely, given `mypackage` and `dependency-1.0.2`, should I write `dependency >= 1` (no upper bounds) or `dependency >= 1 && < 1.1` ([PVP](Package versioning policy) upper bounds). I came to the conclusion that it depends on whether you have an automated bounds-bumping service, and if you do then both the bounds above are wrong, it should be `dependency >= 1 && dependency <= 1.0.2`.

**Rock vs Hard Place**

The reason the debate has continued so long is because both choices are unpleasant:

* Add PVP upper bounds, and have valid install plans rejected and users needlessly downgraded to old versions of packages which are inferior. If one package requires a minimum version of above _n_, and another requires a maximum below _n_, they can't be combined. 
* Don't add upper bounds, and have packages break because they are no longer compatible. Of course, even if dependent packages add new functions/instances they might still break even with PVP compliant upper bounds, and some people violate PVP.

I would argue that the two relevant factors in choosing are how likely the code is to be broken by a newer version, and how long it will take to update the `.cabal` file.

Let us assume that the `.cabal` file can be updated in minutes. If there are excessively restrictive bounds for a few minutes it doesn't matter - the code will be out of date, but only by a few minutes, and new packages requiring the latest version are unlikely.

As the `.cabal` file takes longer to update, the problems with restrictive bounds get worse. For abandoned projects, the restrictive upper bounds make them unusable, but even recently written code can break very fast. However, most code has a habit of working with newer versions of dependencies, so the argument towards no upper bounds gets increasingly strong as the probability of failures decreases and the latency increases. The fact that two variable factors combine to produce a binary decision is likely the reason for such strong debate.

**The Answer**

Throughout, I assume that all package authors would like to support the latest version of all dependencies, that packages can be automatically tested in some way (e.g. Travis) and that packages are supported by semi-engaged authors. Packages for which those conditions do not hold aren't really packages anyone should be relying upon.

Assuming these conditions, you want 


**The Tool**

The tool we need would:

* Monitor packages for upper-bound violations.
* Send a pull request.


Both these states are unpleasant, but which to pick depends on two factors:

* What the probability of a release failing in each way is.
* How long the window of breakage is.

In particular, I would argue that if the window of potential breakage is short (on the order of minutes) then adding upper bounds is the right thing to do - if a package builds with upper bounds 10 minutes ago, it probably will continue to do so for the next 10 minutes. In contrast, if the window of breakage is long (on the order of days) it is far more likely that packages requiring new functionality will be released and you will be using old code. I believe the argument that if the window is short you should use upper bounds is compelling, but that if the window is long the argument you shouldn't use them is weaker.

