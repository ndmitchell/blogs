# The HLint Match Engine

_Summary: HLint has a match engine which powers most of the rules._

The [Haskell linter HLint](https://github.com/ndmitchell/hlint) has two forms of lint - some are built in written in Haskell code over the GHC AST (e.g. [unused extension detection](https://github.com/ndmitchell/hlint/blob/fe9c3ad66968840fcd61a9b967ffd9a89ff2970d/src/Hint/Extensions.hs)), but 700+ hints are written using a matching engine. As an example, we can replace `map f (map g xs)` with `map (f . g) xs`. Doing so might be more efficient, but importantly for HLint, it's often clearer. That rule [is defined in HLint](https://github.com/ndmitchell/hlint/blob/fe9c3ad66968840fcd61a9b967ffd9a89ff2970d/data/hlint.yaml#L129) as:

```yaml
- hint: {lhs: map f (map g x), rhs: map (f . g) x}
```

All single-letter variables are wildcard matches, so the above rule will match:

```haskell
map isDigit (map isUpper "test")
```

And suggest:

```haskell
map (isDigit . isUpper) "test"
```

However, Haskell programmers are uniquely creative in specifying functions - with a huge variety of `$` and `.` operators, infix operators etc. The [HLint matching engine](https://github.com/ndmitchell/hlint/blob/fe9c3ad66968840fcd61a9b967ffd9a89ff2970d/src/GHC/Util/Unify.hs) in HLint v3.1.4 would match this rule to all of the following (I'm using `sort` as a convenient function, replacing it with `foo` below would not change any matches):

* `map f . map g`
* `sort . map f . map g . sort`
* `concatMap (map f . map g)`
* `map f (map (g xs) xs)`
* ``f `map` (g `map` xs)``
* `map f $ map g xs`
* `map f (map g $ xs)`
* `map f (map (\x -> g x) xs)`
* `Data.List.map f (Prelude.map g xs)`
* `map f ((sort . map g) xs)`

That's a large variety of ways to write a nested `map`. In this post I'll explain _how_ HLint matches everything above, and the bug that used to cause it to match even the final line (which _isn't_ a legitimate match) which was fixed in HLint v3.1.5.

**Eta-contraction**

Given a hint comprising of `lhs` and `rhs`, the first thing HLint does is determine if it can eta-contract the hint, producing a version without the final argument. If it can do so for both sides, it generates a completely fresh hint. In the case of `map f (map g x)` in generates:

```yaml
- hint: {lhs: map f . map g, rhs: map (f . g)}
```

For the examples above, the first three match with this eta-contracted version, and the rest match with the original form. Now we've generated two hints, it's important that we don't perform sufficiently fuzzy matching that _both_ match some expression, as that would generate twice as many warnings as appropriate.

**Root matching**

The next step is root matching, which happens only when trying to match at the root of some match. If we have `(foo . bar) x` then it would be reasonable for that to match `bar x`, despite the fact that `bar x` is not a subexpression. We overcome that by transforming the expression to `foo (bar x)`, unifying only on `bar x`, and recording that we need to add back `foo .` at the start of the replacement.

**Expression matching**

After splitting off any extra prefix, HLint tries to unify the single-letter variables with expressions, and build a substitution table with type `Maybe [(String, Expr)]`. The substitution is `Nothing` to denote the expressions are incompatible, or `Just` a mapping of variables to the expression they matched. If two expressions have the same structure, we descend into all child terms and match further. If they don't have the same structure, but are similar in a number of ways, we adjust the source expression and continue.

Examples of adjustments include expanding out `$`, removing infix application such as ``f `map` x`` and ignoring redundant brackets. We translate `(f . g) x` to `f (g x)`, but _not_ at the root - otherwise we might match both the eta-expanded and non-eta-expanded variants. We also re-associate `(.)` where needed, e.g. for expressions like `sort . map f . map g . sort` the bracketing means we have `sort . (map f . (map g . sort))`. We can see that `map f . map g` is not a subexpression of that expression, but given that `.` is associative, we can adjust the source.

When we get down to a terminal name like `map`, we use the scope information HLint knows to determine if the two `map`'s are equivalent. I'm not going to talk about that too much, as it's [slated to be rewritten](https://github.com/ndmitchell/hlint/issues/1001) in a future version of HLint, and is currently both slow and a bit approximate.

**Substitution validity**

Once we have a substitution, we see if there are any variables which map to multiple distinct expressions. If so, the substitution is invalid, and we don't match. However, in our example above, there are no duplicate variables so any matching substitution must be valid.

**Side conditions**

Next we check any side conditions - e.g. we could decide that the above hint only makes sense if `x` is atomic - i.e. does not need brackets in any circumstance. We could have expressed that with `side: isAtom x`, and any such conditions are checked in a fairly straightforward manner.

**Substitutions**

Finally, we substitute the variables into the provided replacement. When doing the replacement, we keep track of the free variables, and if the resulting expression has more free variables than it started with, we assume the hint doesn't apply cleanly. As an example, consider the hint `\x -> a <$> b x` to `fmap a . b`. It looks a perfectly reasonable hint, but what if we apply it to the expression `\x -> f <$> g x x`. Now `b` matches `g x`, but we are throwing away the `\x` binding and `x` is now dangling, so we reject it.

When performing the substitution, we used knowledge of the AST we want, and the brackets required to parse that expression, to ensure we insert the right brackets, but not too many.

**Bug [#1055](https://github.com/ndmitchell/hlint/issues/1055)**

Hopefully all the above sounds quite reasonable. Unfortunately, at some point, the root-matching lost the check that it really was at the root, and started applying the translation to terms such as `sort .` in `map f ((sort . map g) xs)`. Having generated the `sort .`, it decided since it wasn't at the root, there was nowhere for it to go, so promptly threw it away. Oops. HLint v3.1.5 fixes the bug in two distinct ways (for defence in depth):

1. It checks the root boolean before doing the root matching rule.
2. If it would have to throw away any extra expression, it fails, as throwing away that expression is certain to lead to a correctness bug.

**Conclusion**

The matching engine of HLint is relatively complex, but I always assumed one day would be replaced with a finite-state-machine scanner that could match _n_ hints against an expression in _O(size-of-expression)_, rather than the current _O(n * size-of-expression)_. However, it's never been the bottleneck, so I've kept with the more direct version.

I'm glad HLint has a simple external lint format. It allows easy contributions and makes hint authoring accessible to everyone. For large projects it's easy to define your own hints to capture common coding patterns. When using languages whose linter does not have an external matching language (e.g. [Rust's Clippy](https://github.com/rust-lang/rust-clippy)) I certainly miss the easy customization.
