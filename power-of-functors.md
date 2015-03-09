# Implementing a Functor instance

_Summary: Implementing a Functor instance is much easier than implementing a Monad instance, and can turn out to be quite useful._

Haskell forces all programmers to understand some details of the `Monad` typeclass to do basic IO, but currently nothing forces people to learn the `Functor` typeclass. However, `Functor` is much simpler than `Monad`, and all `Monad`s must be `Functor`s, so thinking more about `Functor` can be a nice route to understanding `Monad` better.

An intuitive description of a functor is:

> A container whose contents I can replace, without changing the shape of the container.

Some example functors include lists and `Maybe`. Both contain values, and you can replace the values inside them. In fact, most values with a single type parameter can be made functors. For example, in [CmdArgs](link) I define something similar to:

    data Group a = Group {groupUnnamed :: [a], groupNamed :: [(String, [a])]}

This `Group` structure contains `a` values inside it. Sometimes it is useful to transform all the underlying `a` values, perhaps to a different type. The `Functor` instance has a single member:

    fmap :: Functor f => (a -> b) -> f a -> f b

For the above type, we instantiate `f` to `Group` so we get:

    fmap :: (a -> b) -> Group a -> Group b

We can implement `fmap` by applying `f` to every `a` value inside `Group`:

    instance Functor Group where
        fmap f (Group a b) = Group (map f a) [(x, map f y) | (x,y) <- b]

Note in particular that `Group` is usually written `Group a`, but in the instance declaration we're omitting the `a`, to say `Group` itself (without any arguments) is a functor. Providing insufficient type arguments like that makes `Functor` a higher-kinded type class, in contrast to those like `Eq` or `Ord` which would have been on `Group a`.

When implementing `fmap` the type checker eliminates most bad implementations,  so the only law you need to think about is that `fmap id = id` - given the identity function, the value shouldn't change. We can show this law for `Group` with:

    Group a b = fmap id (Group a b)
    -- inline fmap
    Group a b = Group (map id a) [(x, map id y) | (x,y) <- b]
    -- map id x ==> x
    Group a b = Group a [(x, y) | (x,y) <- b]
    -- simplify list comprehension
    Group a b = Group a b
    -- equal

In fact, the function `map` is just `fmap` specialised to `[]`, so the rule `map id x ==> x` is just applying the `fmap id = id` law on lists. From this law, [we can derive](https://www.fpcomplete.com/user/edwardk/snippets/fmap) the additional law that: 

    fmap (f . g)  ==  fmap f . fmap g

Both these laws can serve as the basis for optimisation opportunities, reducing the number of times we traverse a value, and GHC exploits these laws for the list type.

In general, most data types that take a type parameter can be made functors, but there are a few common exceptions:

* You have a value on the left of an arrow – for example `data Foo a = Foo (a -> Int)` cannot be made a functor, since we have no way to change the our incoming `b` back to an `a`.
* You have an invariant relating the structure and the elements. For example `data OrdList a = Nil | Gt a (OrdList a)`, where all functions on `OrdList` have an `Ord` context, and `OrdList` is exported abstractly. Here the functor would break the abstraction.
* You require an instance for the element type, e.g. `Data.Vector.Storable` requires a `Storable` instance to create a vector, which `Functor` does not provide.

The name functor may sound scary, or confusing to C++ programmers (who accidentally say functor to mean function) – but they are a nice simple abstraction.
