# More space leaks: Alex/Happy edition

_Summary: Alex and Happy had three space leaks, now fixed._

Using the techniques described [in my previous blog post](http://neilmitchell.blogspot.com/2015/09/detecting-space-leaks.html) I checked [`happy`](https://hackage.haskell.org/package/happy) and [`alex`](https://hackage.haskell.org/package/alex) for space leaks. As expected, both had space leaks. Three were clear and unambiguous space leaks, two were more nuanced. In this post I'll describe all five, starting with the obvious ones.

**1: Happy - non-strict accumulating fold**

Happy contains the code:

```
indexInto :: Eq a => Int -> a -> [a] -> Maybe Int
indexInto _ _ []                 = Nothing
indexInto i x (y:ys) | x == y    = Just i
                     | otherwise = indexInto (i+1) x ys
```

This code finds the index of an element in a list, always being first called with an initial argument of 0. However, as it stands, the first argument is a classic space leak - it chews through the input list, building up an equally long chain of `+1` applications, which are only forced later.

The fix is simple, change the final line to:

```
let j = i + 1 in j `seq` indexInto j x ys
```

Or (preferably) switch to using the space-leak free `Data.List.elemIndex`.

**2: Happy - sum using foldr**

Happy also contained the code:

```
foldr (\(a,b) (c,d) -> (a+b,b+d)) (0,0) conflictList
```

The first issue is that the code is using `foldr` to produce a small atomic value, when `foldl'` would be a much better choice. Even after switching to `foldl'` we still have a space leak because `foldl'` only forces the outer-most value - namely just the pair, not the `Int` values inside. We want to force the elements inside the pair so are forced into the more painful construction:

```
foldl' (\(a,b) (c,d) -> let ac = a + c; bd = b + d in ac `seq` bd `seq` (ac,bd)) (0,0) conflictList
```

Not as pleasant, but it does work. In some cases people may prefer to define the auxiliary:

```
let strict2 f !x !y = f x y
in foldr (\(a,b) (c,d) -> strict (,) (a+b) (b+d)) (0,0) conflictList
```

**3: Alex - lazy state in a State Monad**

Alex features the code:

```
N $ \s n _ -> (s, addEdge n, ())
```

Here `N` roughly corresponds to a state monad with 2 fields, `s` and `n`. In this code `n` is a `Map`, which operates strictly, but the `n` itself is not forced until the end. We solve the problem by forcing the value before returning the triple:

```
N $ \s n _ -> let n' = addEdge n in n' `seq` (s, n', ())
```

**4: Alex - array freeze**

Alex calls the `Data.Array.MArray.freeze` function, to convert an `STUArray` (unboxed mutable array in the `ST` monad) into a `UArray` (unboxed immutable array). Unfortunately the `freeze` call in the `array` library uses an amount of stack proportional to the size of the array. Not necessarily a space leak, but not ideal either. Looking at the code, it's also very inefficient, constructing and deconstructing lots of intermediate data. Fortunately under normal optimisation a rewrite rule fires for this type to replace the call with one to [`freezeSTUArray`](https://hackage.haskell.org/package/array-0.5.1.1/docs/src/Data.Array.Base.html#freezeSTUArray), which is much faster and has bounded stack, but is not directly exported.

Usually I diagnose space leaks under `-O0`, on the basis that any space leak problems at `-O0` may eventually cause problems anyway if an optimisation opportunity is lost. In this particular case I had to `-O1` that module.

**5: Happy - complex fold**

The final issue occurs in a function `fold_lookahead`, which when given lists of triples does an `mconcat` on all values that match in the first two components. Using the [`extra` library](https://github.com/ndmitchell/extra#readme) that can be written as:

```
map (\((a,b),cs) -> (a,b,mconcat cs)) . groupSort . map (\(a,b,c) -> ((a,b),c))
```

We first turn the triple into a pair where the first two elements are the first component of the pair, call `groupSort`, then `mconcat` the result. However, in Happy this construction is encoded as a `foldr` doing an insertion sort on the first component, followed by a linear scan on the second component, then individual `mappend` calls. The `foldr` construction uses lots of stack (more than 1Mb), and also uses _O(n^2)_ algorithms instead of _O(n log n)_.

Alas, the algorithms are not identical - the resulting list is typically in a different order. I don't believe this difference matters, and the tests all pass, but  it does make the change more dangerous than the others.


**The result**

After these changes we use < 1Kb of stack.
