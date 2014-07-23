# Applicative vs Monadic build systems

_Summary: Shake is a monadic build system, and monadic build systems are more powerful than applicative ones._

Several [people](https://twitter.com/tomaspetricek/status/245149652150808577) [have](http://www.reddit.com/r/haskell/comments/plv2b/neil_mitchell_shake_a_better_make/c3qercr) [wondered](http://www.reddit.com/r/haskell/comments/18tn88/how_does_shake_build_lib_work/c8hx51w) if the dependencies in the [Shake build system](https://github.com/ndmitchell/shake#readme) are monadic, and if Make dependencies are applicative. In this post I'll try and figure out what that means, and show that the claim is somewhat true.

Gergo [recently wrote](http://gergo.erdi.hu/blog/2014-07-12-arrow's_place_in_the_applicative/monad_hierarchy/) a good primer on the concepts of Applicative, Monads and Arrows (it is worth reading the first half if you are unfamiliar with monad or applicative). Using a similar idea, we can model a simple build system as a set of rules:

    rules :: [(FilePath, Action String)]
    rules = [("a+b", do a <- need "a"; b <- need "b"; return (a ++ b))
            ,("a"  , return "Hello ")
            ,("b"  , return "World")
            ]

Each rule is on a separate line, containing a pair of the file the rule produces (e.g. `a` for the second rule) and the action that produces the files contents (e.g. `return "Hello"`). I've used `need` to allow a rule to use the contents of another file, so the rule for `a+b` depends on the files `a` and `b`, then concatenates their contents. We can run these rules to produce all the files. We've written these rules assuming `Action` is a `Monad`, using the `do` notation for monads. However, for the above build system, we can restrict ourselves to `Applicative` functions:

    rules = [("a+b", (++) <$> need "a" <*> need "b")
            ,("a"  , pure "Hello ")
            ,("b"  , pure "World")
            ]

If `Action` is applicative but not monadic then we can statically (without running any code operating on file contents) produce a dependency graph. If `Action` is monadic we can't generate a graph upfront, but there are some build systems that cannot be expressed applicatively. In particular, using a monad we can write a "dereferencing" build system:

    rules = [("!a", do a <- need "a"; need a)
            ,("a" , pure "b")
            ,("b" , pure "Goodbye")
            ]

To build the file `!a` we first require the file `a` (which produces the contents `b`), then we require the file `b` (which produces the contents `Goodbye`). Note that the first rule has changed `b` _the content_ into `b` _the file name_. In general, to move information from the file content to a file name, requires a monad. Alternatively stated, a monad lets you chose future dependencies based on the results of previous dependencies.

One realistic example (from the original [Shake paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf)), is building a `.tar` file from the list of files contained in a file. Using Shake we can write the `Action`:

    contents <- readFileLines "list.txt"
    need contents
    cmd "tar -cf" [out] contents

The only build systems that I'm aware of that are monadic are [redo](https://github.com/apenwarr/redo), [SCons](http://www.scons.org/) and Shake-inspired build systems (including [Shake itself](https://github.com/ndmitchell/shake#readme), [Jenga](https://github.com/janestreet/jenga) in [OCaml](http://ocaml.org/), and several Haskell alternatives).

While it is the case that Shake is monadic, and that monadic build systems are more powerful than applicative ones, it is _not_ the case that Make is applicative. In fact, almost no build systems are purely applicative. Looking at the [build shootout](https://github.com/ndmitchell/build-shootout#monad1-monadic-patterns), every build system tested can implement the `!a` example (provided the file `a` is not a build product), despite several systems being based on applicative dependencies.

Looking at Make specifically, it's clear that the `output: input1 input2` formulation of dependencies _is_ applicative in nature. However, there are at least two aspects I'm aware of that increase the power of Make:

* Using `$(shell cat list.txt)` I can splice the contents of `list.txt` into the Makefile, reading the contents of `list.txt` before the dependencies are parsed.
* Using `-include file.d` I can include additional rules that are themselves produced by the build system.

It seems every "applicative" build system contains some mechanism for extending its power. I believe some are strictly less powerful than monadic systems, while others may turn out to be an encoding of monadic rules. However, I think that an explicitly monadic definition provides a clearer foundation.
