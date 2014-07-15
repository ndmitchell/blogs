# The Power of Functor

_Summary: Functor is a bit like Monad, but much simpler. Unfortunately it tends to get ignored as Monads are required for IO, but it’s actually rather useful._

One type class that sometimes gets overlooked by intermediate Haskell programmers is Functor. Functor has some similarities to Monad – they are both higher-kinded (see below) and taken from category theory. Haskell forces all programmers to understand some details of Monad to do basic IO, but there is nothing that requires learning functors. However, Functor is much simpler than Monad, has some commonalities that might help understanding Monads, and is useful in its own right.

An intuitive description of a functor is:

* A container whose contents I can replace, without changing the shape of the container.

Some example functors include lists and Maybe. Both contain values, and contain ways to replace the values inside them. But in fact, most values with a single type parameter can be made functors (there are exceptions, see below). For example, in CmdArgs I defined something similar to:

    data Group a = Group {groupUnnamed :: [a], groupNamed :: [(String, [a])]}

This Group structure contains a’s inside it. It’s useful to operate on all the underlying a values and transform them. If a /= String, and I am doing an a->a transformation, then I could use Uniplate transform instead. However, a functor is much more natural (but not always applicable). It’s easy to write a functor instance:

    instance Functor Group where
        fmap f (Group a b) = Group (map f a) [(x, map f b) | (x,y) <- b]

The different thing to Eq/Ord is that this is a higher-kinded type class. The Group type usually takes 1 type argument, but here we have given it none.

In general, if you have a data type that takes a type parameter, and stores that parameter inside, then you can probably create a functor instance. If you do create a functor instance, it will probably come in use.

Functors are also much simpler in terms of laws. There are two laws:

    fmap id  ==  id
    fmap (f . g)  ==  fmap f . fmap g

That property simply means that if you leave the children the same, the outer structure will not change. That is usually easy to check. The difficult part is that you have transformed all the children exactly once – but fortunately the type checker perfectly captures that invariant (thanks to theorems for free).
 
If you have a 1 element structure, I know of two cases where you might not want a functor instance:

* You have a value on the left of an arrow – for example data Foo a = Foo (a -> Int) cannot be made a functor, since we have no way to change the our incoming b back to an a.
* You have an invariant relating the structure and the elements. For example data OrdList a = Nil | Gt a (OrdList a), where all functions on the list have an Ord context, and OrdList is exported abstractly. Here the functor would break the abstraction.

The name function may sound scary, or confusing to C++ programmers (where functor means function) – but they are a nice simple abstraction.

