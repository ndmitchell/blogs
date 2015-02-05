# Refactoring with Equational Reasoning

_Summary: Haskell is great for refactoring, thanks to being able to reason about and transform programs with confidence._

I think one of Haskell's strengths as a practical language is that it's easy to refactor, and more importantly, easy to refactor safety. Programs in the real world often accumulate technical debt - code that is shaped more by its history than its current purpose. Refactoring is one way to address that technical debt, making the code simpler, but not changing any meaningful behaviour.

When refactoring, you need to think of which alternative forms of code are equivalent but better. In C++ I've removed unused variables, only to find they were [RAII](http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization) variables, and their mere presence had a semantic effect. In R I've removed redundant `if` expressions, only to find the apparently pure condition had the effect of coercing a variable and changing its type. In Haskell, it's equally possible to make refactorings that at first glance appear safe but aren't - however, in practice, it happens a lot less. I think there are a few reasons for that:

* Haskell is pure and evaluation strategy is largely unobservable - moving a statement "before" or "after" another lexically is usually safe.
* Refactorings that do go wrong, for example variables that accidentally get moved out of scope or types which are no longer as constrained, usually result in compiler errors.
* The Haskell community cares about semantics and laws. The [Monad laws](https://wiki.haskell.org/Monad_laws) are satisfied by almost all monads, flagrantly breaking those laws is rare.  
* Functions like `unsafePerformIO`, which could harm refactoring, are almost always used behind a suitable pure abstraction.

Note that these reasons are due to both the language, and the conventions of the Haskell community. (Despite these factors, there are a few features that can trip up refactorings, e.g. exceptions, record wildcards, space-leaks.)

To take a very concrete example, today I was faced with the code:

    f = fromMaybe (not b) . select
    if f v == b then opt1 else opt2

At one point the function `f` was used lots, had a sensible name and nicely abstracted some properties. Now `f` is used once, the semantics are captured elsewhere, and the code is just unclear. We can refactor this statement, focusing on the condition:

    f v == b
    -- inline f
    (fromMaybe (not b) . select) v == b
    -- remove brackets and inline (.)
    fromMaybe (not b) (select v) == b
    -- expand to a case statement
    (case select v of Nothing -> not b; Just x -> x) == b
    -- push the == down
    case select v of Nothing -> not b == b; Just x -> x == b
    -- simplify not b == b
    case select v of Nothing -> False; Just x -> x == b
    -- collapse back up
    select v == Just b

And now substitute back in:

    if select v == Just b then opt1 else opt2

Our code is now much simpler and more direct. Thanks to the guarantees I expect of Haskell programs, I also have a high degree of confidence this code really is equivalent - even if it isn't obvious just looking at beginning and end.

