# Thoughts on Conduits

_Summary: I'm growing increasingly fond of the Conduit library. Here I give my intuitions and some hints I'd have found useful._

Recently I've been working on converting the [Hoogle database generation](https://github.com/ndmitchell/hoogle) to use the [Conduit abstraction](https://hackage.haskell.org/package/conduit), in an effort to reduce the memory and improve the speed. It worked - database generation has gone from 2Gb of RAM to 320Mb, and time has dropped from several minutes (or > 15 mins on memory constrained machines) to 45s. These changes are all in the context of [Hoogle 5](http://neilmitchell.blogspot.co.uk/2015/01/hoogle-5-is-coming.html), which should hopefully be out in a month or so.

The bit that I've converted to Conduit is something that takes in a tarball of one text files per Hackage file, namely the Haddock output with one definition per line ([this 22Mb file](https://hackage.haskell.org/packages/hoogle.tar.gz)). It processes each definition, saves it to a single binary file (with compression and some processing), and returns some compact information about the definition for later processing. I don't expect the process to run in constant space as it is accumulating some return information, but it is important that most of the memory used by one definition is released before the next definition. I originally tried lazy IO, and while it somewhat worked, it was hard to abstract properly and very prone to space leaks. Converting to Conduit was relatively easy and is simpler and more robust.

### The Conduit model

My mental model for a conduit `Conduit a m b` is roughly a function `[a] -> m [b]` - `a` values go in and `b` values come out (but interleaved with the monadic actions). More concretely you ask for an `a` with `await` and give back a `b` with `yield`, doing stuff in the middle in the `m` Monad.

A piece of conduit is always either running (doing it's actual work), waiting after a `yield` for the downstream person to ask for more results (with `await`), or waiting after an `await` for the upstream person to give the value (with `yield`). You can think of a conduit as making explicit the demand-order inherent in lazy evaluation.

### Things to know about Conduit

I think it's fair to say Conduit shows its history - this is good for people who have been using it for a while (your code doesn't keep breaking), but bad for people learning it (there are lots of things you don't care about). Here are some notes I made:

* The [`Data.Conduit`](https://hackage.haskell.org/package/conduit/docs/Data-Conduit.html) module in the [`conduit`](https://hackage.haskell.org/package/conduit) library _is not_ the right module to use - it seems generally accepted to use the [`Conduit`](https://hackage.haskell.org/package/conduit-combinators/docs/Conduit.html) module from the [`conduit-combinators`](https://hackage.haskell.org/package/conduit-combinators) package. However, I decided to build my own `conduit-combinators` style replacement in the Hoogle tree, see [General.Conduit](https://github.com/ndmitchell/hoogle/blob/master/src/General/Conduit.hs) - the standard `Conduit` module has a lot of dependencies, and a lot of generalisations.
* Don't use `Source` or `Sink` - use `Producer` and `Consumer` - the former are just a convenient way to get confusing error messages.
* Don't use `=$` or `$=`, always use `=$=` between conduits. The `=$=` operator is essentially `flip (.)`.
* Given a `Conduit` you can run it with `runConduit`. Alternatively, given `a =$= b =$= c` you can replace any of the `=$=` with `$$` to run the Conduit as well. I find that a bit ugly, and have stuck to `runConduit`.
* `Conduit` and `ConduitM` have their type arguments in different orders, which is just confusing. However, generally I use either `Conduit` (a connector) or `Producer` (something with a result). You rarely need something with a result and a return value.
* You call `await` to see if a value is available to process. The most common bug I've had with conduits is forgetting to make the function processing items recursive - usually you want `awaitForever`, not just `await`.
* The ByteString lines Conduit function was accidentally _O(n^2)_ - I spotted and [fixed that](https://github.com/snoyberg/conduit/pull/209). Using difference lists does not necessarily make your code _O(n)_!

### Useful functions

When using Conduit I found a number of functions seemingly missing, so defined them myself.

First up is `countC` which counts the number of items that are consumed. Just a simple definition on top of `sumC`.

    countC :: (Monad m, Num c) => Consumer a m c
    countC = sumC <| mapC (const 1)

While I recommend `awaitForever` in preference to `await`, it's occasionally useful to have `awaitJust` as the single-step `awaitForever`, if you are doing your own recursion.

    awaitJust :: Monad m => (i -> Conduit i m o) -> Conduit i m o
    awaitJust act = do
        x <- await
        whenJust x act

I regularly find `zipFrom i = zip [i..]` very useful in strict languages, and since Conduit can be viewed as a strict version of lazy lists (through very foggy glasses) it's no surprise a Conduit version is also useful.

    zipFromC :: (Monad m, Enum c) => c -> Conduit a m (c, a)
    zipFromC !i = awaitJust $ \a -> do
        yield (i,a)
        zipFromC (succ i)

Finally, it's useful to `zip` two conduits. I was surprised how fiddly that was with the standard operators (you have to use `newtype` wrappers and an `Applicative` instance), but a simple `|$|` definition hides that complexity away.

    (|$|) :: Monad m => ConduitM i o m r1 -> ConduitM i o m r2 -> ConduitM i o m (r1,r2)
    (|$|) a b = getZipConduit $ (,) <$> ZipConduit a <*> ZipConduit b

### Why not Pipes?

I am curious if all Pipes users get asked "Why not use Conduit?", or if this FAQ is asymmetrical?

I realise pipes are billed as the more "principled" choice for this type of programming, but I've yet to see anywhere Conduit seems fundamentally unprincipled. I use [WAI](https://hackage.haskell.org/package/wai)/[Warp](https://hackage.haskell.org/package/warp) and [http-conduit](https://hackage.haskell.org/package/http-conduit), so learning Conduit gives me some help there.
