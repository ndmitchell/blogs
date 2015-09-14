# Making `sequence`/`mapM` for `IO` take _O(1)_ stack

_Summary: We have a version of `mapM` for `IO` that takes _O(1)_ stack and is faster than the standard Haskell/GHC one for long lists._

The standard Haskell/GHC base library `sequence` function in `IO` takes _O(n)_ stack space. However, working with Tom Ellis, we came up with a version that takes _O(1)_ stack space. In reality, our version is slower at reasonable sizes, but faster at huge sizes (100,000+ elements). The standard definition of `sequence` (when specialised for both `IO` and `[]`) is equivalent to:

    sequence :: [IO a] -> IO [a]
    sequence [] = return []
    sequence (x:xs) = do y <- x; ys <- sequence xs; return (y:ys)

Or, when rewritten inlining the monadic bind and opening up the internals of GHC's IO type, becomes:

    sequence :: [IO a] -> IO [a]
    sequence [] = IO $ \r -> (# r, () #)
    sequence (y:ys) = IO $ \r -> case unIO y r of
        (# r, y #) -> case unIO (sequence xs) r of
            (# r, ys #) -> (# r, y:ys #)

For those not familiar with `IO`, it is internally defined as:

    newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #)

Each `IO` action takes a `RealWorld` token and returns a `RealWorld` token, which ensures that `IO` actions run in order. See [here]() for a full tutorial.

Our observation was that this version requires _O(n)_ stack space, as each recursive call is performed inside a `case`. The algorithm proceeds in two phases:

* First, it traverses the input list, evaluating each action and pushing `y` on the stack.
* After reaching the `[]` at the end of the list, it traverses the stack constructing the output list.

By constructing the list directly on the heap we can avoid the extra copy and use _O(1)_ stack. Our version is:

    sequenceIO :: [IO a] -> IO [a]
    sequenceIO xs = do
            ys <- IO $ \r -> (# r, apply r xs #)
            evaluate $ demand ys
            return ys
        where
            apply r [] = []
            apply r (IO x:xs) = case x r of
                (# r, y #) -> y : apply r xs

            demand [] = ()
            demand (x:xs) = demand xs

Here the two traversals are explicit:

* First, we traverse the list using `apply`. Note that we pass the `RealWorld` token down the list (ensuring the items happen in the right order), but we do not pass it back.
* To ensure all the `IO` actions performed during `apply` happen before we return any of the list, we then `demand` the list, ensuring the `[]` element has been forced.

Both these traversals use _O(1)_ stack. The first runs the actions and constructs the list. The second ensures evaluation has happened before we continue. The trick in this algorithm is:

    ys <- IO $ \r -> (# r, apply r xs #)

Here we cheat by duplicating `r`, which is potentially unsafe. This line does not evaluate `apply`, merely returns a thunk which when evaluated will force `apply`, and cause the `IO` to happen during evaluation, at somewhat unspecified times. To regain well-defined evaluation order we force the result of `apply` on the next line using `demand`.

**Benchmarks**

We benchmarked using GHC 7.10.2, comparing the default `sequence` (which has identical performance to the specialised monomorphic variant at the top of this post), and our `sequenceIO`. We benchmarked at different lengths of lists. Our `sequenceIO` is twice as slow at short lists, draws even around 10,000-100,000 elements, and goes 40% faster by 1,000,000 elements.

Our algorithm saves allocating stack, at the cost of iterating through the list twice. It is likely that by tuning the stack allocation flags the standard algorithm would be faster everywhere.

**Using `sequence` at large sizes**

Despite improved performance at large size, we would not encourage people to use `sequence` or `mapM` at such large sizes - these functions still require _O(n)_ memory. Instead:

* If you are iterating over the elements, consider fusing this stage with subsequence stages, so that each element is processed one-by-one. The `conduit` and `pipes` libraries both help with that approach.
* If you are reducing the elements, e.g. performing a `sum`, consider fusing the `mapM` and `sum` using something like `foldM`.

For common operations, such a `concat` after a `mapM`, an obvious definition of `concatMapM` is:

    concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
    concatMapM f = liftM concat . mapM f

But that if the result of the argument is regularly `[]` then a more efficient version is:

    concatMapM op = foldr f (return [])
        where f x xs = do x <- op x
                          if null x then xs else liftM (x ++) xs

For lists of 10,000 elements long, using the function `const (return [])`, this definition is about 4x faster. [Version 1.4.2 of the extra library](https://hackage.haskell.org/package/extra) uses this approach for both `concatMapM` and `mapMaybeM`.
