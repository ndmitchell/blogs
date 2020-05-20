# GHC Unproposals

_Summary: Four improvements to Haskell I'm not going to raise as GHC proposals._

Writing a [GHC proposal](https://github.com/ghc-proposals/ghc-proposals) is a lot of hard work. It requires you to fully flesh out your ideas, and then defend them robustly. That process can take [many months](https://neilmitchell.blogspot.com/2018/12/ghc-from-bug-to-merge.html). Here are four short proposals that I won't be raising, but think would be of benefit (if you raise a proposal for one of them, I'll buy you a beer next time we are physically co-located).

**Use `:` for types**

In Haskell we use `:` for list-cons and `::` for types. That's the [wrong way around](https://neilmitchell.blogspot.com/2018/11/counting-cost-of-colons-in-haskell.html) - the use of types is increasing, the use of lists is decreasing, and type theory has always used `:`. This switch has been [joke-proposed before](https://github.com/ghc-proposals/ghc-proposals/pull/118). We actually switched these operators [in DAML](https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816), and it worked very nicely. Having written code in both styles, I now write Haskell on paper with `:` for types instead of `::`. Nearly all other languages use `:` for types, [even Python](https://docs.python.org/3/library/typing.html). It's sad when Python takes the more academically pure approach than Haskell.

_Is it practical_: Maybe. The compiler diff is quite small, so providing it as an option has very little technical cost. The problem is it bifurcates the language - example code will either work with `:` for types or `::` for types. It's hard to write documentation, text books etc. If available, I would switch my code.

**Make recursive `let` explicit**

Currently you can write `let x = x + 1` and it means loop forever at runtime because `x` is defined in terms of itself. You probably meant to refer to the enclosing `x`, but you don't get a type error, and often don't even get a good runtime error message, just a hang. In `do` bindings, to avoid the implicit reference to self, it's [common to write](https://neilmitchell.blogspot.com/2020/03/the-pure-pattern.html) `x <- pure $ x + 1`. That can impose a runtime cost, and obscure the true intent.

In languages like OCaml there are two different forms of `let` - one which allows variables to be defined and used in a single `let` (spelled `let rec`) and one which doesn't (spelled `let`). Interestingly, this distinction is important in GHC Core, which has two different keywords, and a source `let` desugars differently based on whether it is recursive. I think Haskell should add `letrec` as a separate keyword and make normal `let` non-recursive. Most recursive bindings are done under a `where`, and these would continue to allow full recursion, so most code wouldn't need changing.

_Is it practical_: The simplest version of this proposal would be to add `letrec` as a keyword equivalent to `let` and add a [warning on recursive `let`](https://gitlab.haskell.org/ghc/ghc/issues/14527). Whether it's practical to go the full way and redefine the semantics of `let` to mean non-recursive binding depends on how strong the adoption of `letrec` was, but given that I suspect recursive `let` is less common, it seems like it could work. Making Haskell a superset of GHC Core is definitely an attractive route to pursue.

**Allow trailing variables in bind**

When writing Haskell code, I often have `do` blocks that I'm in the middle of fleshing out, e.g.:

```
do fileName <- getLine
   src <- readFile fileName
```

My next line will be to print the file or similar, but this entire `do` block, and every sub-part within it, is constantly a parse error until I put in that final line. When the IDE has a parse error, it can't really help me as much as I'd like. The reason for the error is that `<-` can't be the final line of a `do` block. I think we should relax that restriction, probably under a language extension that only IDE's turn on. It's not necessarily clear [what such a construct should mean](https://twitter.com/ndm_haskell/status/1223215216739201030), but in many ways that isn't the important bit, merely that such a construct results in a valid Haskell program, and allows more interactive feedback.

_Is it practical_: Yes, just add a language extension - since it doesn't actually enable any new power it's unlikely to cause problems. Fleshing out the semantics, and whether it applies to `let x = y` statements in a `do` block too, is left as an exercise for the submitter. An alternative would be to not change the language, but make GHC emit the error slightly later on, much like `-fdefer-type-errors`, which still works for IDEs (either way needs a GHC proposal).

**Add an exporting keyword**

Currently the top of every Haskell file duplicates all the identifiers that are exported - unless you just export everything (which you shouldn't). That approach duplicates logic, makes refactorings like renamings more effort, and makes it hard to immediately know if the function you are working on is exposed. It would be much nicer if you could just declare things that were exported inline, e.g. with a `pub` keyword - so `pub myfunc :: a -> a` both defines and exports `myfunc`. Rust has [taken this approach](https://doc.rust-lang.org/reference/visibility-and-privacy.html) and it works out quite well, modulo [some mistakes](https://twitter.com/ndm_haskell/status/1254015262451535874). The currently Haskell design has been found a bit wanting, with constructs like `pattern Foo` in the export list to differentiate when multiple names `Foo` might be in scope, when attaching the visibility to the identifier would be much easier.

_Is it practical_: Perhaps, provided someone doesn't try and take the proposal too far. It would be super tempting to differentiate between exports to of the package, and exports that are only inside this package (what Rust clumsily calls `pub(crate)`). And there are other things in the module system that could be improved. And maybe we should export submodules. I suspect everyone will want to pile more things into this design, to the point it breaks, but a simple exporting keyword would probably be viable.
