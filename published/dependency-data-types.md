# Data types for build system dependencies

_Summary: Monadic and early cut-off? Use a sequence of sets._

In the [Build Systems a la Carte paper](https://ndmitchell.com/#shake_21_apr_2020) we talk about the expressive power of various types of build systems. We deliberately simplify away parallelism and implementation concerns, but those details matter. In this post I'm going to discuss some of those details, specifically the representation of dependencies.

## Applicative build systems

In an applicative build system like Make, all dependencies for a target are known before you start executing the associated action. That means the dependencies have no ordering, so are best represented as a set. However, because they can be calculated from the target, they don't usually need to be stored separately. The dependencies can also be evaluated in parallel. To build a target you evaluate the dependencies to values, then evaluate the action.

Early cut-off is when an action is skipped because none of its dependencies have changed _value_, even if some dependencies might have required recomputing. This optimisation can be incredibly important for build systems with generated code - potentially [seconds vs hours of build time](https://ndmitchell.com/#shake_10_sep_2012). To obtain early cut-off in applicative systems, after evaluating the dependencies you compare them to the previous results, and only run the action if there were changes.

## Monadic build systems

In monadic [build systems like Shake](https://shakebuild.com/), the representation of dependencies is more complex. If you have an alternative mechanism of detecting whether a rule is dirty (e.g. reverse dependencies) you don't need to record the dependencies at all. If the key is dirty, you start executing the action, and that will request the dependencies it needs. The action can then suspend, calculate the dependencies, and continue.

If you want early cut-off in a monadic build system, you need to rerun the dependencies in advance, and if they all have the same result, skip rerunning the action. Importantly, you probably want to rerun the dependencies in the _same order_ that the action originally requested them -- otherwise you might pay a severe and unnecessary time penalty. As an example, let's consider an action:

```haskell
opt <- need "is_optimised"
object <- if opt then need "foo.optimised" else need "foo.unoptimised"
link object
```

This rule is monadic, as whether you need the optimised or unoptimised dependency depends on the result of calculating some `is_optimised` property. If on the first run `is_optimised` is `True`, then we build `foo.optimised`. On the second run, if `is_optimised` is `False`, it is important we _don't_ build `foo.optimised` as that might take a seriously long time and be entirely redundant. Therefore, it's important when checking for early cut-off we build in the order that the previous action requested the dependencies, and stop on the first difference we encounter.

(If you have unlimited resources, e.g. [remote execution](https://docs.bazel.build/versions/master/remote-execution.html), it might be profitable to evaluate everything in parallel - but we're assuming that isn't the case here.)

Provided a rule performs identically between runs (i.e. is deterministic and hasn't been changed), everything that we request to check for early cut-off will still be needed for real, and we won't have wasted any work. For all these reasons, it is important to store dependencies as a sequence (e.g. a list/vector).

## Monadic build systems plus parallelism

Applicative build systems naturally request all their dependencies in parallel, but monadic build systems are naturally one dependency at a time. To regain parallelism, in build systems like Shake the primitive dependency requesting mechanism takes a _set_ of dependencies that are computed in parallel. While requesting dependencies individually or in bulk gives the same result, in bulk gives significantly more parallelism. (In Shake we use lists to track correspondence between requests and results, but it's morally a set.)

As we saw previously, it is still important that for early cut-off you reproduce the dependencies much like they were in the action. That means you request dependencies in the order they were requested, and when they were requested in bulk, they are also checked in bulk. Now we have a sequence of sets to represent dependencies, where the elements of the sets can be checked in parallel, but the sequence must be checked in order.

## Monadic build systems plus explicit parallelism

What if we add an explicit parallelism operator to a monadic build system, something like `parallel :: [Action a] -> IO [a]` to run arbitrary actions in parallel (which is what Shake provides). Now, instead of a sequence of sets, we have a _tree_ of parallelism. As before it's important when replaying that the dependencies are requested in order, but also that as much is requested in parallel as possible.

## What Shake does

Shake is a monadic build system with early cut-off, parallelism and explicit parallelism. When building up dependencies it uses a tree representation. The full data type is:

```haskell
data Traces
    = None
    | One Trace
    | Sequence Traces Traces
    | Parallel [Traces]
```

Sequenced dependencies are represented with `Sequence` and the traces captured by parallelism use `Parallel`. Importantly, constructing `Traces` values is nicely _O(1)_ in all cases. (Shake v0.19.1 used a different representation and repeatedly normalised it, which could have awful time complexity - potentially _O(2^n)_ in pathological cases.)

While these traces store complete information, actually evaluating that trace when checking for rebuilds would be complicated. Instead, we flatten that representation to `[[Trace]]` for writing to the Shake database. The outer list is a sequence, the inner list is morally a set. We have the invariant that no `Trace` value will occur multiple times, since if you depend on something once, and then again, the second dependency was irrelevant. To flatten `Parallel` computations we take the first required dependency in each parallel action, merge them together, and then repeat for the subsequent actions. If you run code like:

```haskell
parallel [
    need ["a"] >> parallel [need ["b"], need ["c"]]
    need ["d"]
]
```

It will get flattened to appear as though you wrote `need ["a","d"] >> need ["b","c"]`. When checking, it will delay the evaluation of `b` and `c` until after `d` completes, even though that is unnecessary. But simplifying traces at the cost of marginally less rebuild parallelism for those who use explicit parallelism (which is not many) seems like the right trade-off for Shake.

## Conclusion

Applicative build systems should use sets for their dependencies. Monadic build systems should use sets, but if they support early cut-off, should use sequences of sets.
