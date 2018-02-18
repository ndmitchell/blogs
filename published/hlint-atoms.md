# Atomic Expressions Generically

_Summary: For certain hints HLint needs to determine if a Haskell expression is atomic. I wrote a generic method to generate expressions and test if they are atomic._

With [HLint](https://github.com/ndmitchell/hlint#readme), if you write a statement such as:

```haskell
main = print ("Hello")
```

You get the hint:

```console
Sample.hs:1:14: Warning: Redundant bracket
Found:
  ("Hello")
Why not:
  "Hello"
```

One of ways HLint figures out if brackets are redundant is if the expression inside the brackets is "atomic" - if you never have to bracket it in any circumstances. As an example, a literal string is atomic, but an `if` expression is not. The [`isAtom` function from `haskell-src-exts-util`](https://hackage.haskell.org/package/haskell-src-exts-util/docs/Language-Haskell-Exts-Util.html#v:isAtom) has a list of the types of expression which are atomic, but the [`Exp` type from `haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts/docs/Language-Haskell-Exts-Syntax.html#t:Exp) has 55 distinct constructors, and I don't even know what many of them do. How can we check the `isAtom` function is correct?

One approach is to use human thought, and that's the approach used until now, with reasonable success. However, I've recently [written a script](https://github.com/ndmitchell/haskell-src-exts-util/blob/master/script/CheckIsAtom.hs) which solves the problem more permanently, generating random expressions and checking that `isAtom` gives the right value. In this post I'm going to outline a few features of how that script works. There are basically three steps:

**1) Generate a type-correct `Exp`**

The first step is to generate a random `Exp` which follows the type definition. Fortunately the `Data` class in Haskell lets us generate values. We define:

```haskell
mkValue :: forall a . Data a => Int -> IO a
mkValue depth
    | Just x <- cast "aA1:+" = randomElem x
    | Just x <- cast [-1 :: Int, 1] = randomElem x
    | Just x <- cast [-1 :: Integer, 1] = randomElem x
    | AlgRep cs <- dataTypeRep $ dataTypeOf (undefined :: a) =
        if depth <= 0 then throwIO LimitReached else fromConstrM (mkValue $ depth - 1) =<< randomElem cs
```

Here we are saying that given a `depth`, and a result type `a`, we generate a value of type `a`. Note that the `a` argument is the result, but we don't pass anything in of type `a`. The first three lines of the body follow the pattern:

```haskell
    | Just x <- cast [list_of_element] = randomElem x
```

This tries to convert `list_of_element` to `[a]` by using runtime type information. If it succeeds, we pick a random element from the list. If it doesn't we continue onwards.

The final case uses `dataTypeRep`/`dataTypeOf` to get a list of the constructors of `a`. Note that we don't have a value of `a`, so we make one up using `undefined :: a` - but that's OK because `dataTypeOf` promises not to look at its argument. Given a list of constructors, we pick one at random, and then call `fromConstrM` - which says how to create a value of the right constructor, using some argument to fill in all the fields. We pass `mkValue` as that argument, which causes us to recursively build up random values.

One immediate problem is what if we are building a `[Int]` and the random generator often picks `(:)`? We'll take a very long time to finish. To solve this problem we keep a `depth` counter, decrement it in every recursive call, and when it runs out, `throwIO` an exception and give up.

**2) Generate a parsing `Exp`**

Now we've got a valid `Exp` value, but just because an `Exp` can be represented in the AST doesn't mean it corresponds to Haskell fragment. As an example, consider `Var (UnQual (Ident "Test"))`. That's a valid value of type `Exp`, but if you pretty print it you get `Test`, and if you parse it back you'll get `Con (UnQual (Ident "Test"))` - variables must start with a leading lower-case letter.

To ignore invalid expressions we try pretty printing then parsing the expression, and ignore all expressions which don't roundtrip.

**3) Determine if the `Exp` is atomic**

Now we've got a valid `Exp`, which we know the user could have typed in as a source program, we need to figure out if `isAtom` is correct. To do that we see if given expression `x` whether self-application roundtrips, i.e. `x x`. As a positive example, `foo` (a variable) roundtrips as `foo foo` being `foo` applied to itself. However, `if b then t else f` when applied to itself gives `if b then t else f if b then t else f`, which parses back more like `if b then t else f (if b then t else f)`, and is not atomic.

**Putting it all together**

Now we've got a random expression, and we know if the atomicity agrees with what we were expecting, we can report any differences. That approach has identified many additional patterns to match, but it's not perfect, in particular:

* Most values either exceed the depth limit or fail to roundtrip. For 10,000 `if` expressions I typically get 1 or 2 which roundtrip properly. For non-`if` expressions it's usually 100 or so. The advantage of random testing is that throwing more time at a problem solves such issues without thinking too hard.
* For some expressions, e.g. `ParComp`, I've never managed to get a valid value created. Perhaps `haskell-src-exts` can't parse it, or perhaps it requires constants I don't have in my hardcoded list - none of these were particularly common examples.
* `haskell-src-exts` has a bug where `-1` is pretty printed as `(-1)`, which is then parsed as a paren and `-1`. That fails step 2, so we don't test with negative literals. As it happens, non-negative literals are atomic, but negative literals aren't, so we need to take care.
* There are some patterns which appear to roundtrip successfully on their own, but not when surrounded by brackets, but secretly are just very weird. For example `do rec\n   []` parses successfully, but with source positions that are error values, and when applied to itself pretty prints incorrectly. There's at least one `haskell-src-exts` bug here.
* The program appears to leak progressively more memory. I solved that by running slices of it at a time, and didn't look too hard. I've seen cases of blowup in `Data` constructors when recursing, so it could be that. but needs investigating.

As a result of all this work a future HLint will spot unnecessary brackets for 20 more types of expression, 8 more types of pattern and 7 more types of type.
