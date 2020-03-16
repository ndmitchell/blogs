# The `<- pure` pattern

_Summary: Sometimes `<- pure` makes a lot of sense, avoiding some common bugs._

In Haskell, in a monadic `do` block, you can use either `<-` to bind monadic values, or `let` to bind pure values. You can also use `pure` or `return` to wrap a value with the monad, meaning the following are mostly equivalent:

```
let x = myExpression
x <- pure myExpression
```

The one place they aren't fully equivalent is when `myExpression` contains `x` within it, for example:

```
let x = x + 1
x <- pure (x + 1)
```

With the `let` formulation you get an infinite loop which never terminates, whereas with the `<- pure` pattern you take the previously defined `x` and add `1` to it. To solve the infinite loop, the usual solution with `let` is to rename the variable on the left, e.g.:

```
let x2 = x + 1
```

And now make sure you use `x2` everywhere from now on. However, `x` remains in scope, with a more convenient name, and the same type, but probably shouldn't be used. Given a sequence of such bindings, you often end up with:

```
let x2 = x + 1
let x3 = x2 + 1
let x4 = x3 + 1
...
```

Given a large number of unchecked indicies that must be strictly incrementing, bugs usually creep in, especially when refactoring. The unused variable warning will sometime catch mistakes, but not if a variable is legitimately used twice, but one of those instances is incorrect.

Given the potential errors, when a variable `x` is morally "changing" in a way that the old `x` is not longer useful, I find it much simpler to write:

```
x <- pure myExpression
```

The compiler now statically ensures we haven't fallen into the traps of an infinite loop (which is obvious and frustrating to track down) or using the wrong data (which is much harder to track down, and often very subtly wrong).

**What I really want:** What I actually think Haskell should have done is made `let` non-recursive, and had a special `letrec` keyword for recursive bindings (leaving `where` be recursive by default). This distinction is present in GHC Core, and would mean `let` was much safer.

**What HLint does:** HLint is very aware of the `<- pure` pattern, but also aware that a lot of beginners should be guided towards `let`. If any variable is defined more than once on the LHS of an `<-` then it leaves the `do` alone, otherwise it will suggest `let` for those where it fits.
