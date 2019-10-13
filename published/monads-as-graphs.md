# Monads as Graphs

_Summary: You can describe type classes like monads by the graphs they allow._

In the [Build Systems a la Carte paper](https://github.com/snowleopard/build/releases/download/icfp-final/build-systems.pdf) we described build systems in terms of the type class their dependencies could take. This post takes the other view point - trying to describe type classes (e.g. `Functor`, `Applicative`, `Monad`) by the graphs they form.

**Functor**

The [`Functor` class](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Functor) has one operation: given `Functor m`, we have `fmap :: (a -> b) -> m a -> m b`. Consequently, if we want to end up with an `m b`, we need to start with an `m a` and apply `fmap` to it, and can repeatedly apply multiple `fmap` calls. We can visualise that with:

We've used circles for the values `m a` and lines to represent the `fmap` that connects them. `Functor` supplies no operations to "merge" two circles, so our dependencies form a linear tree. Thinking as a build system, this represents [Docker](https://docs.docker.com/engine/reference/commandline/build/), where base images can be extended to form new images.

**Applicative**

The [`Applicative` class](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Applicative) has two operations - `pure :: a -> m a` (which we ignore because its pretty simple) and `liftA2 :: (a -> b -> c) -> m a -> m b -> m c` (in reality `<$>` is actually in the class - but `liftA2` is equivalent in power).  Thinking from a graph perspective, we now have the ability to create a graph node that points at _two_ children, and uses the function argument to `liftA2` to merge them. Since `Applicative` is a superset of `Functor`, we still have the ability to point at _one_ child if we want. Children can also be pointed at by multiple parents, which just corresponds to reusing a value. We can visualise that with:

The structure of an `Applicative` graph can be calculated _before_ any values on the graph have been calculated, which can be more efficient for tasks like [parsing](https://stackoverflow.com/a/7863380/160673) or [build systems](https://medium.com/@Shuayb1/topological-sorting-efe4d7b542a6). When viewed as a build system, this represents build systems like [Make](https://www.gnu.org/software/make/) or [Buck](https://buck.build/), where all dependencies are given up front.

**Selective**

The next type class we look at is [`Selective`](https://hackage.haskell.org/package/selective/docs/Control-Selective.html#t:Selective), which can be characterised with the operation `ifS :: m Bool -> m a -> m a -> m a`. From a graph perspective, `Selective` interrogates the _value_ of the first node, and then selects either the second or third node. We can visualise that as:

Unlike before, we don't know exactly what the final graph structure will be until we have computed the value on the first node of `ifS`. However, we can statically over-approximate the graph by assuming both branches will be taken. In build system terms, this graph corresponds to something like [Dune](https://github.com/ocaml/dune).

**Monad**

The final type class is [`Monad`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Monad) which can be characterised with the operation `(>>=) :: m a -> (a -> m b) -> m b`. From a graph perspective, `Monad` interrogates the _value_ of the first node, and then does whatever it likes to produce a second node. It can point at some existing node, or create a brand new node using the information from the first. We can visualise that as:

The use of an arrow pointing nowhere seems a bit odd, but it represents the unlimited options that the `Monad` provides. Before we always knew all the possible structures of the graph in advance. Now we can't know anything beyond a monad-node at all. As a build system, this graph represents a system like [Shake](https://shakebuild.com/).
