# `foldr` under the hood

_Summary: The `foldr` function seems simple, but is actually very complex, with lots of layers. This post dives through the layers._

The `foldr` function takes a list and replaces all  `:` (cons) and `[]` (nil) values with functions and a final value. It's available in the [Haskell `Prelude`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldr) and described [on Wikipedia](https://en.wikipedia.org/wiki/Fold_(higher-order_function)). As some examples:

```
sum = foldr (+) 0
map f = foldr (\x xs -> f x : xs) []
```

But the simple `foldr` described on Wikipedia is many steps away from the one in the Haskell `Prelude`. In this post we'll peel back the layers, learning why `foldr` is a lot more complicated under the hood.

**Layer 1: Wikipedia definition**

The definition on Wikipedia is:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)
```

This recursive definition directly describes what `foldr` does. Given a list `[1,2,3]` we get `f 1 (f 2 (f 3 z))`.

**Layer 2: Static argument transformation**

The problem with this definition is that it is recursive, and [GHC doesn't like to inline recursive functions](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf), which prevents a lot of optimisation. Taking a look at `sum`, it's a real shame that operations like `(+)` are passed as opaque higher-order functions, rather than specialised to the machine instruction `ADD`. To solve that problem, [GHC defines `foldr` as](https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#foldr):

```
foldr f z = go
    where go []     = z
          go (x:xs) = f x (go xs)
```

The arguments `f` and `z` are constant in all sucessive calls, so they are lifted out with a manually applied [static argument transformation](https://gitlab.haskell.org/ghc/ghc/wikis/static-argument-transformation).

Now the function `foldr` is no longer recursive (it merely has a `where` that _is_ recursive), so `foldr` can be inlined, and now `+` can meet up with `go` and everything can be nicely optimised.

**Layer 3: Inline later**

We now have `foldr` that can be inlined. However, inlining `foldr` is not always a good idea. In particular, GHC has an optimisation called list fusion based on the idea that combinations of `foldr` and `build` can be merged, sometimes known as [short-cut deforestation](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf). The basic idea is that if we see `foldr` applied to `build` we can get rid of both (see [this post](https://teh.id.au/posts/2017/06/30/notes-on-fusion/index.html) for details). We remove `foldr` using the [GHC rewrite rule](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#rewrite-rules):

```
{-# RULES "my foldr/build" forall g k z. foldr k z (build g) = g k z #-}
```

The most interesting thing about this rule (for this post at least!) is that it matches `foldr` _by name_. Once we've inlined `foldr` we have thrown away the name, and the rule can't fire anymore. Since this rule gives significant speedups, we really want it to fire, so GHC adds [an extra pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#inline-pragma) to `foldr`:

```
{-# INLINE [0] foldr #-}
```

This `INLINE` pragma says don't try and inline `foldr` until the final stage of the compiler, but in that final stage, be very keen to inline it.

**Layer 4: More polymorphism**

However, the `foldr` function in the `Prelude` is not the one from [`GHC.List`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-List.html), but actually a more general one that works for anything [`Foldable`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Foldable). Why limit yourself to folding over lists, when you can fold over other types like `Set`. So now `foldr` is generailsed from `[]` to `t` with:

```
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

Where `foldr` on `[]` is `GHC.List.foldr`.

**Layer 5: A default implementation**

But `foldr` is actually in the type class `Foldable`, not just defined on the outside. Users defining `Foldable` can define only `foldr` and have all the other methods defined for them. But they can equally define only `foldMap`, and have an implicit version of `foldr` defined as:

```
foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo . f) t) z
```

Where `Endo` is defined as:

```
newtype Endo = Endo {appEndo :: a -> a}

instance Monoid (Endo a) where
    mempty = Endo id
    Endo a <> Endo b = Endo (a . b)
```

The function `foldMap f` is equivalent to `mconcat . map f`, so given a list `[1,2,3]` the steps are:

* First apply `map (Endo . f)` to each element to get `[Endo (f 1), Endo (f 2), Endo (f 3)]`.
* Next apply `mconcat` to the list to get `Endo (f 1) <> Endo (f 2) <> Endo (f 3)`.
* Inline all the `<>` definitions to get `Endo (f 1 . f 2 . f 3)`.
* Apply the `appEndo` at the beginning and `z` at the end for `(f 1 . f 2 . f 3) z`.
* Inline all the `.` to give `f 1 (f 2 (f 3 z))`, which is what we had at layer 1.

**Layer 6: Optimising the default implementation**

The real default implementation of `foldr` is:

```
foldr f z t = appEndo (foldMap (Endo #. f) t) z
```

Note that the `.` after `Endo` has become `#.`. Let's first explain why it's correct, then why it might be beneficial. The definition of `#.` is:

```
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
```

Note that it has the same type as `.` (plus a `Coercible` constraint), but ignores it's first argument entirely. The [`coerce` function](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/coercible.pdf) transforms a value of type `a` into a value of type `b` with zero runtime cost, provided they have the same underlying representation. Since `Endo` is a `newtype`, that means `Endo (f 1)` and `f 1` are implemented identically in the runtime, so `coerce` switches representation "for free". Note that the first argument to `#.` only serves to pin down the types, so if we'd passed an interesting function as the first argument it would have been ignored.

Of course, in normal circumstances, a `newtype` is free anyway, with no runtime cost. However, in this case we don't have a `newtype`, but a function application with a `newtype`. You can see the gory details in [GHC ticket 7542](https://gitlab.haskell.org/ghc/ghc/issues/7542), but at one point this impeeded other optimisations.

I tried a simulated version of `foldr` and found that if GHC can't tell that `[]` is the `Foldable` the code looks pretty bad, but if it can, at `-O1` and above, the two implementations are 100% equialent (to the point that common subexpression elimination makes them actually the same). It's possible this final layer is a vestigial optimisation, or perhaps it's still important in some circumstances.
