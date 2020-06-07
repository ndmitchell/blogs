# Surprising IO: How I got a benchmark wrong

_Summary: IO evaluation caught me off guard when trying to write some benchmarks._

I once needed to know a quick back-of-the-envelope timing of a pure operation, so hacked something up quickly rather than going via [`criterion`](https://hackage.haskell.org/package/criterion). The code I wrote was:

```haskell
main = do
    (t, _) <- duration $ replicateM_ 100 $ action myInput
    print $ t / 100

{-# NOINLINE action #-}
action x = do
    evaluate $ myOperation x
    return ()
```

Reading from top to bottom, it takes the time of running `action` 100 times and prints it out. I deliberately engineered the code so that GHC couldn't optimise it so `myOperation` was run only once. As examples of the defensive steps I took:

* The `action` function is marked `NOINLINE`. If `action` was inlined then `myOperation x` could be floated up and only run once.
* The `myInput` is given as an argument to `action`, ensuring it can't be applied to `myOperation` at compile time.
* The `action` is in `IO` so the it has to be rerun each time.

Alas, GHC still had one trick up its sleeve, and it wasn't even an optimisation - merely the definition of evaluation. The `replicateM_` function takes `action myInput`, which is evaluated once to produce a value of type `IO ()`, and then runs that `IO ()` 100 times. Unfortunately, in my benchmark `myOperation x` is actually evaluated in the process of creating the `IO ()`, not when running the `IO ()`. The fix was simple:

```haskell
action x = do
    _ <- return ()
    evaluate $ myOperation x
    return ()
```

Which roughly desugars to to:

```haskell
return () >>= \_ -> evaluate (myOperation x)
```

Now the `IO` produced has a lambda inside it, and my benchmark runs 100 times. However, at `-O2` GHC used to manage to break this code once more, by lifting `myOperation x` out of the lambda, producing:

```haskell
let y = myOperation x in return () >>= \_ -> evaluate y
```

Now `myOperation` runs just once again. I finally managed to defeat GHC by lifting the input into `IO`, giving:

```
action x = do
    evaluate . myOperation =<< x
    return ()
```

Now the input `x` is itself in `IO`, so `myOperation` can't be hoisted.

I originally wrote this post a very long time ago, and back then GHC did lift `myOperation` out from below the lambda. But nowadays it doesn't seem to do so (quite possibly because doing so might cause a space leak). However, there's nothing that promises GHC won't learn this trick again in the future.
