# Existential Serialisation

_Summary: Using static pointers you can perform binary serialisation of existentials._

Many moons ago [I asked](https://stackoverflow.com/questions/8101067/binary-instance-for-an-existential/45984547) how to write a `Binary` instance for a type including an existential, such as:

    data Foo = forall a . (Typeable a, Binary a) => Foo a

Here we have a constructor `Foo` which contains a value. We don't statically know the type of the contained value, but we do know it has the type classes `Typeable` (so we can at runtime switch on its type) and `Binary` (so we can serialise it). But how can we deserialise it? We can store the relevant `TypeRep` when serialising, but when deserialising there is no mechanism to map from `TypeRep` to a `Binary` instance.

In [Shake](http://shakebuild.com), I needed to serialise existentials, as described in [the S4.1 of the original paper](http://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf). My solution was to build a global mapping table, storing pairs of `TypeRep` and `Binary` instances for the types I knew were relevant. This solution works, but cannot deserialise anything that has not already been added to the global table, which required certain functions to live in weird places to ensure that they were called before deserialisation. Effective, but ugly.

Recently [Abhiroop Sarkar](https://stackoverflow.com/users/1942289/abhiroop-sarkar) suggested using the relatively new [static pointers extension](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/static-pointers.html). This extension lets you turn top-level bindings with no arguments into a `StaticPtr` which can then be serialised/deserialsed, even between different instances of a process. To take advantage of this feature, we can redefine `Foo` as:

	data Foo = forall a . (StaticFoo a, Binary a) => Foo a

	class StaticFoo a where
	    staticFoo :: a -> StaticPtr (Get Foo)

The approach is to switch from serialising the `TypeRep` (from which we try to look up `Get Foo`), to serialising the `Get Foo` directly. We can write a `Binary Foo` instance by defining `put`:

    put :: Foo -> Put
    put (Foo x) = do
        put $ staticKey $ staticFoo x
        put x

Here we simply grab a `StaticPtr (Get Foo)` which can deserialise the object, then use `staticKey` to turn it into something that can be serialised itself. Next, we write out the payload. To reverse this process we define `get`:

    get :: Get Foo
    get = do
        ptr <- get
        case unsafePerformIO (unsafeLookupStaticPtr ptr) of
            Just value -> deRefStaticPtr value :: Get Foo
            Nothing -> error "Binary Foo: unknown static pointer"

We first `get` the `staticKey`, use `unsafeLookupStaticPtr` to turn it into a `StaticPtr (Get Foo)` followed by `deRefStaticPtr` to turn it into a `Get Foo`. The `unsafe` prefix on these functions is justified - bugs while developing this code resulted in segfaults.

The final piece of the puzzle is defining `StaticFoo` instances for the various types we might want to serialise. As an example for `String`:

    instance StaticFoo String where
        staticFoo _ = static (Foo <$> (get :: Get String))

We perform the `get`, wrap a `Foo` around it, and then turn it into a `StaticPtr`. All other types follow the same pattern, replacing `String` with `Int` (for example). The expression passed to `static` must have no free variables, including type variables, so we cannot define an instance for `a`, or even an instance for `[a]` - it must be `[Char]` and `[Int]` separately.

A complete code sample and test case is available [here](https://gist.github.com/ndmitchell/a4f2edcedd2d4398efea4755b5d2408f).

This approach works, and importantly allows extra constraints on the existential. The main disadvantage is that `static` isn't very flexible or easy to abstract over, resulting in a lot of `StaticFoo` boilerplate.

Will Shake be moving over to this approach? No. The next version of Shake has undergone an extensive rewrite, and in the process, moved away from needing this feature. A problem I had for 8 years has been solved, just as I no longer need the solution!
