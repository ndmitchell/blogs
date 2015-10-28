# ViewPatterns and lambda expansion

_Summary: One of HLint's rules reduced sharing in the presence of view patterns. Lambda desugaring and optimisation could be improved in GHC._

[HLint](https://hackage.haskell.org/package/hlint) has the rule:

    function x = \y -> body
        ==>
    function x y = body

Given a function whose body is a lambda, you can use the function syntactic sugar to move the lambda arguments to the left of the `=` sign. One side condition is that you can't have a `where` binding, for example:

    function x = \y -> xx + y
        where xx = trace "hit" x

This is equivalent to:

    function x = let xx = trace "hit" x in \y -> xx + y

Moving a `let` under a lambda can cause arbitrary additional computation, as [I previously described](http://neilmitchell.blogspot.co.uk/2011/09/sharing-in-haskell.html), so is not allowed (hint: think of `map (function 1) [2..5]`).

### View Patterns

One side condition I hadn't anticipated is that if `x` is a view pattern, the transformation can still reduce sharing. Consider:

    function (trace "hit" -> xx) = \y -> xx + y

This is equivalent to:

    function x = case trace "hit" x of xx -> \y -> xx + y

And moving `y` to the right of the `=` causes `trace "hit"` to be recomputed for every value of `y`.

I've now fixed HLint 1.9.22 to spot this case. Using [Uniplate](https://hackage.haskell.org/package/uniplate), I added the side condition:

    null (universeBi pats :: [Exp_])

Specifically, there must be no expressions inside the pattern, which covers the `PViewPat` constructor, and any others that might harbour expressions (and thus computation) in future.

The problem with function definitions also applies equally to `\p1 -> \p2 -> e`, which cannot be safely rewritten as `\p1 p2 -> e` if `p1` contains a view pattern.  

### The Problem Worsens (Pattern Synonyms)

[Pattern synonyms](https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms) make this problem worse, as they can embody arbitrary computation in a pattern, which is lexically indistinguishable from a normal constructor. As an example:

    pattern Odd <- (odd -> True)
    f Odd = 1
    f _ = 2

However, putting complex computation behind a pattern is probably not a good idea, since it makes it harder for the programmer to understand the performance characteristics. You could also argue that using view patterns and lambda expressions to capture computation after partial application on definitions then lambda expressions is also confusing, so I've [refactored Shake](https://github.com/ndmitchell/shake/commit/f1a7779773f61539f7c0509943b0424919c16fa7) to avoid that. 

### Potential Fixes

I think it should be possible to fix the problem by optimising the desugaring of functions, ensuring patterns are matched left-to-right where allowable, and that each match happens before the lambda requesting the next argument. The question is whether such a change would improve performance generally. Let's take an example:

    test [1,2,3,4,5,6,7,8,9,10] x = x
    test _ x = negate x

Could be changed to:

    test [1,2,3,4,5,6,7,8,9,10] = \x -> x
    test _ = trace "" $ \x -> negate x

Which goes 3-4x faster when running `map (test [1..]) [1..n]` at `-O2` (thanks to [Jorge Acereda Maciá](https://github.com/jacereda) for the benchmarks). The `trace` is required to avoid GHC deoptimising the second variant to the first, as per [GHC bug #11029](https://ghc.haskell.org/trac/ghc/ticket/11029#comment:1).

There are two main downsides I can think of. Firstly, the desugaring becomes more complex. Secondly, these extra lambdas introduce overhead, as the [STG machine](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode) GHC uses makes multiple argument lambdas cheaper. That overhead could be removed using call-site analysis and other optimisations, so those optimisations might need improving before this optimisation produces a general benefit.
