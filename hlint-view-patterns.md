# ViewPatterns and lambda expansion

_Summary: One of HLint's rules reduced sharing in the presence of view patterns._

[HLint](https://hackage.haskell.org/package/hlint) has the rule:

    function x = \y -> body
        ==>
    function x y = body

Given a function whose body is a lambda, you can use the function syntactic sugar to move the lambda arguments to the left of the `=` sign. One side condition is that you can't have a `where` binding, for example:

    function x = \y -> xx + y
        where xx = trace "hit" x

This is equivalent to:

    function x = let xx = trace "hit" x in \y -> xx + y

Moving a `let` under a lambda can cause arbitrary additional computation, as [I previously described](), so is not allowed (hint: think of `map (function 1) [2..5]`).

### View Patterns

One side condition I hadn't anticipated is that if `x` has a view pattern, the transformation can still reduce sharing. Consider:

    function (trace "hit" -> xx) = \y -> xx + y

This is equivalent to:

    function x = case trace "hit" x of xx -> \y -> xx + y

And again, moving the `y` to the right of the `=` causes `trace "hit"` to be recomputed for every value of `y`.

I've now fixed HLint to detect this side condition. Using [Uniplate](https://hackage.haskell.org/package/uniplate), I added the side condition:

    null (universeBi pats :: [Exp_])

Specifically, there must be no expressions inside the pattern, which covers the `PViewPat` constructor, and any others that might harbour expressions (and thus computation) in future.

### The Problem Worsens

Unfortunately, pattern synonyms are going to make this problem worse, as they can embody arbitrary computation in a pattern, which is lexically indistinguishable from a normal constructor. As an example:

    Look up on the web

However, putting complex computation behind a pattern is probably not a wise idea, since it makes optimising harder for the programmer.

The problem also applies to `\p1 -> \p2 -> ...`, which cannot be rewritten as `\p1 p2 -> ...` if `p1` contains a view pattern.  

### Potential Fixes

I think it should be possible to fix the problem by optimising the desugaring of functions, ensuring patterns are matched left-to-right where allowable, and that each match happens before the lambda requesting the next argument. The question is whether such a change would improve performance generally. Let's take an example:

    test [1,2,3,4,5,6,7,8,9,10] x = x
    test _ x = negate x

Can be changed to:

    test [1,2,3,4,5,6,7,8,9,10] = \x -> x
    test _ = \x -> negate x

Running `map (test [1..]) [1..n]` goes over 50x faster with the second variant. Simplifying the pattern to `(_:_)` still results in the second variant going 3x faster.

The downside is that these extra lambdas introduce overhead, but overhead that can be removed using call-site analysis and other optimisations (ones that traditionally weren't that good in GHC, but which are getting better).
