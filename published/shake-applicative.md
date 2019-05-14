# Shake with Applicative Parallelism

_Summary: Shake now does that Applicative trick from Haxl._

In Shake 0.17.9 and below, `need xs >> need ys` builds `xs` in parallel, then afterwards builds `ys` in parallel. The same is true of `need xs *> need ys`, where `*>` is the applicative equivalent of `>>`. From Shake 0.18 onwards both versions run everything in parallel. Hopefully that makes some Shake-based build systems go faster.

**What change is being made?**

If you make two calls to `apply` without any `IO`, monadic-bind or state operations in between then they will be executed as though you had made a single call to `apply`. As examples, `need`, `askOracle` and `getDirectoryFiles` are all calls to `apply` under the hood, so can be merged. However, note that the invariants are somewhat subtle. Something as simple as:

```
myNeed xs = do putNormal "Needing here"; need xs
```

Will not be merged with a preceeding `need` - the function `putNormal` queries the state (what is the verbosity level), does IO and contains a monadic bind.

**Why are you making this change?**

I am making the change for two reasons: 1) people have kept asking for it since [Haxl](https://hackage.haskell.org/package/haxl) does it; 2) the [Hadrian build](https://neilmitchell.blogspot.com/2019/03/ghc-rebuild-times-shake-profiling.html) probably benefits from it. The downsides are relatively low (more complexity inside Shake, slightly slower `Action` operations) but the benfits are potentially large.

**Why didn't you make this change sooner?**

My previous reasoning for not making the change was:

> Shake _could_ follow the Haxl approach, but does not, mainly because they are targeting different problems. In Haxl, the operations are typically read-only, and any single step is likely to involve lots of operations. In contrast, with Shake the operations definitely change the file system, and there are typically only one or two per rule. Consequently, Shake opts for an explicit approach, rather than allow users to use `*>` (and then inevitably add a comment because its an unusual thing to do).

I stand by that comment - explicit grouping of `need` or explicit use of `parallel` is often better - all it takes is a sneaky `>>=` and the parallelism disappears. But if this change improves some build times, it's hard to argue strongly against.

**Will it break any build systems?**

Potentially, but unlikely, and those it will break were already on thin ice. As some examples:

* If `a` depends on some state change from `b` (e.g. creating a directory), but doesn't have a dependency on it, then `need [a] >> need [b]` might have worked, while `need [a,b]` might not. The correct solution is for `a` to depend on `b`, if it does in fact depend on `b`, or at the very least use `orderOnly`.
* If you use `getDirectoryFiles` on generated files (something the documentation says is a bad idea) then if merged with the thing that generates the files you will get incoherent results. The solution is to avoid using `getDirectoryFiles` on generated files.

_Thanks to [Pepe Iborra](https://github.com/pepeiborra) for encouraging, testing and troubleshooting this change._
