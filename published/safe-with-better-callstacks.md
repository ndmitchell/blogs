# Safe Library with better Stack Traces

_Summary: The `safe` library now provides error messages with short and informative stack traces on errors._

The Haskell [`base` library](https://hackage.haskell.org/package/base) contains a number of functions that crash under certain circumstances, e.g. `tail []`. The [`safe` library](https://hackage.haskell.org/package/safe) attempts to tame these functions, providing `tailDef` (which uses a default value on `[]`) and `tailNote` (which gives you a chance to provide some extra information if there is a failure). Since GHC 7.10 there has been support for adding stack traces to exceptions, where if any function with an appropriate annotation calls `error` it will include some stack trace.

Just over a year ago the `safe` library introduced a `Partial` constraint in `Safe.Partial` to declare that a function is partial, which also provides the annotations for `error`. The signature of `tailNote` became:

```haskell
tailNote :: Partial => String -> [a] -> [a]
```

This signature has two benefits - it is visible in the documentation _and_ it provides the stack trace if something goes wrong. If you typed `tailNote "uhoh" []` in `ghci` you got:

```console
Prelude Safe> tailNote "uhoh" []
*** Exception: Safe.tailNote [], uhoh
CallStack (from HasCallStack):
  error, called at .\Safe\Util.hs:23:44 in safe-0.3.16-9YcgrXj17kg79mfNx7tCoF:Safe.Util
  fromNoteModule, called at Safe.hs:65:12 in safe-0.3.16-9YcgrXj17kg79mfNx7tCoF:Safe
  fromNote, called at Safe.hs:108:17 in safe-0.3.16-9YcgrXj17kg79mfNx7tCoF:Safe
  tailNote, called at <interactive>:5:1 in interactive:Ghci1
```

Great - we can see the final line says we were on line 5 of the interactive and ran `tailNote`. Useful, but with the new version of `safe` it's even better:

```console
*Main> tailNote "uhoh" []
*** Exception: Safe.tailNote [], uhoh
CallStack (from HasCallStack):
  tailNote, called at <interactive>:1:1 in interactive:Ghci1
```

We still get the interesting final line, but all the internal details of `safe`, e.g. the fact that `tailNote` calls `fromNote` have disappeared.

To get the stack traces just add `Partial` to any function you believe to be partial - it's easy. If you are happy to stick with GHC 8.0 and above you can use `HasCallStack` from `GHC.Stack` without depending on `safe`. I am slowly adding annotations to my packages, for example the [`extra` library](https://hackage.haskell.org/package/extra) has `Partial` annotations.

Supressing the internal functions was a lot more work. I think it's worth it for a package that is all about nice handling of errors, but I probably won't bother in any of my other packages. The change to the code was going from:

```haskell
tailNote note = fromNote note "tailNote []" . tailMay
```

To:

```haskell
tailNote note x = withFrozenCallStack $ fromNote note "tailNote []" $ tailMay x
```

The main change is we've added `withFrozenCallStack`, which freezes the call stack at this point and stops new entries from being accumulated inside the bowels of our library. The other change was to eta-expand the definition by adding `x` on both sides, so that `withFrozenCallStack` gets to block the actual error, and not merely a function that later produces an error.

`Partial` constraints are very powerful, and I hope in time they are adopted universally throughout the Haskell ecosystem. One day I hope the `Prelude.tail` function will also have a `Partial` constraint.
