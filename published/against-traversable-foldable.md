# Why Traversable/Foldable should not be in the Prelude

_Summary: For GHC 7.10, Traversable and Foldable are going to be in the Prelude. I missed the original discussion, but I suspect it's a bad idea._

Types are how Haskell programmers communicate their intentions to each other. Currently, the Haskell Prelude contains:

    mapM :: Monad m => (a -> m b) -> [a] -> m [b]

As of GHC 7.10, as part of something known as the Burning Bridges Proposal ([ticket](https://ghc.haskell.org/trac/ghc/ticket/9586), [discussion](http://www.haskell.org/pipermail/libraries/2013-May/019902.html), I can't actually find a full proposal...), that will become:

    mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)

Surely that's a good thing? Aren't more general types always better? Isn't the `Prelude` an archaic beast from the time before? I'd argue functions which are highly polymorphic are hard to use, and hard to think about, especially for beginners. I'd also argue the `Prelude` is remarkably _well designed_, not perfect, but quite an impressive feat.

**What makes a type signature complex?**

I've been thinking recently about what makes type signatures complex, both to practitioners, and to relative beginners. My rough metric is:

* Fully concrete types are usually simple, as long as they aren't too long. The longer a type gets, the more complex it gets.
* Types with functions in them aren't too bad (order-1 types), but as you go up to order-2 types things start to get more complex.
* Fully polymorphic functions can be simpler than concrete functions, since they declare what you don't need to worry about.
* Functions with type classes are more complex, since you need to read the type signature while looking at the context, and need to know each class being used.
* Simple type classes (`Eq`, `Show`) aren't too bad, but custom type classes impose more of a burden.
* As you add more type classes, the complexity grows faster than linearly. Three type classes are not three times as complex as one, but quite a bit harder.
* Higher kinded type classes are significantly more complex than kind `*` type classes, e.g. `Monad`, `Functor`. The reason is that instead of having a hole you fill in, you now have a hole which itself has a hole.
* The higher-kinded type classes `Monad` and `Functor` aren't as bad as the others, since `Functor` is really the "simplest" higher-kinded type class, and `Monad` is required knowledge for IO.
* As you have more higher kinded type classes, the complexity burden grows even worse than for kind `*` type classes. Two is significantly more complex than one.

By that metric, the old `mapM` isn't too bad, but the new `mapM` is quite complex. It has two higher-kinded type classes, and one of them is not one of the common ones. I appreciate that making `Foldable` and `Traversable` key to Haskell will probably lead to them being more used, but now all beginners are going to have to wade through the `Monad` tutorial, their `Foldable` tutorial and their `Traversable` tutorial before they start programming (or just give up).

**Why generality hurts**

There are two main reasons why generality hurts:

_Reading type signatures becomes difficult/impossible._ We already have that problem with the `Control.Arrow` module, which (as far as most people use it), is just a pile of tuple combinators. But unlike other tuple combinators, these are ones whose type signature can't be understood. When I want to use `&&&` or `***` I just pick randomly, see if it type checks, then try again. When other people I know what to use these functions they just use an explicit lambda. No one thinks of referring to the documentation, since the documentation presents a unification problem (which most of the people I know could solve), not an intuition.

_Reading code becomes difficult._ Haskell is brilliant for letting you write a composable pipeline of code that takes some input, does some processing, and produces some output. But that only works if you have enough concrete pieces in each function to read each piece in isolation. As an example:

    test = foo . mapM baz . bar

Using the current `mapM` definition I can, in a fraction of a second, know the approximate shape of what `foo` consumes, and what `bar` produces. With the new `mapM` I don't, and have to keep more context in my head to reason about the code.

**Who it hurts**

Generality of this nature tends to hurt two types of people:

_Beginners_ are hurt because they need to know more concepts just to get going. As a beginner I read through `Data.List` regularly to build up weapons in my arsenal to attack larger problems. The new `Data.List` will be generalised, and reading it won't give the insights I enjoyed. Maybe the beginner can instantiate all `Foldable` things to `[]`, but that adds a mental burden to exactly those people who can bear it least.

_Practitioners_, those who are paid to code for a living, will have greater problems with maintenance. This isn't an unsubstantiated guess... I have taken over a project which made extensive use of the generalised `traverse` and `sequence` functions. Yes, the code was concise, but it was read-only, and even then, required me to "trust" that the compiler and libraries snapped together properly.

**Who it benefits**

The benefit probably comes from those who are already using the `Applicative`/`Traversable` classes regularly. For these people, they can probably avoid an `import Prelude()`. I am not against ever changing the `Prelude`, but I do think that for changes of this magnitude the ideas should probably be prototyped as a separate package, widely accepted, and only then should significant surgery be attempted on the `Prelude`. The [classy-prelude](https://hackage.haskell.org/package/classy-prelude) work has gone in that direction, and I wish them luck, but the significant changes they've already iterated through suggest the design space is quite large.

**Concluding remarks**

I realise that I got to this discussion late, perhaps too late to expect my viewpoint to count. But I'd like to leave by reproducing [Henning Thielemann's email](http://www.haskell.org/pipermail/libraries/2013-May/019820.html) on the subject:

> > David Luposchainsky wrote:
> >
> > +1. I think the Prelude should be a general module of the most commonly
> > needed functions, which (generalized) folds and traversals are certainly
> > part of; right now it feels more like a beginner module at times.
>
> It is certainly a kind of beginner module, but that's good. Experts know 
> how to import. Putting the most general functions into Prelude does not 
> work because:
> 
> 1. There are often multiple sensible generalizations of a Prelude
> function.
> 
> 2. You have to add more type annotations since types cannot be infered 
> from the functions.
> 
> There is simply no need to change Prelude and all packages that rely on 
> specific types. Just don't be lazy and import the stuff you need!
> 
> I should change my vote to:
> 
>   -10

