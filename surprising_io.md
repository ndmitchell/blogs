# Surprising IO: How I got a benchmark wrong

_Summary: IO evaluation caught me off guard when trying to write some benchmarks._

I recently needed to know a quick back-of-the-envelope timing of a pure operation, so hacked something up quickly rather than going via [`criterion`](https://hackage.haskell.org/package/criterion). The code I wrote was:

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

Alas, GHC still had one trick up its sleve, and it wasn't even an optimisation - merely the definition of evaluation. The `replicateM_` function takes `action myInput`, which is evaluated once to produce a value of type `IO ()`, and then runs that `IO ()` 100 times. Unfortunately, in my benchmark `myOperation x` is actually evaluated in the process of creating the `IO ()`, not when running the `IO ()`. My fix was simple:

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

Now the `IO` produced has a lambda inside it, and my benchmark runs 100 times. However, at `-O2` GHC manages to break this code once more, by lifting `myOperation x` out of the lamdba, producing:

```haskell
let y = myOperation x in return () >>= \_ -> evaluate y
```

Now the code runs just once.

evaluate (myOperation x) >> return ()

 calling `action myInput`


 To make sure that GHC doesn't perform common subexpression elimination or let floating I put the `action ` function in `IO` and