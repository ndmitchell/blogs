# Shake 0.16 - revised rule definitions

_Summary: I've just released shake v0.16. A lot has changed, but it's probably only visible if you have defined your own rules or oracles._

Shake-0.16 is now out, 8 months since the last release, and with a lot of improvements. For full details read the [changelog](https://github.com/ndmitchell/shake/blob/master/CHANGES.txt), but in this post I'm going to go through a few of the things that might have the biggest impact on users.

**Rule types redefined**

Since the first version of Shake there has been a `Rule key value` type class defining all rule types - for instance the file rule type has `key` of filename and `value` of modification time. With version 0.16 the type class is gone, rules are harder to write, but offer higher performance and more customisation. For people using the builtin rule types, you'll see those advantages, and in the future see additional features that weren't previously possible. For people defining custom rule types, those will require rewriting - [read the docs](https://hackage.haskell.org/package/shake-0.16/docs/Development-Shake-Rule.html) and if things get tough, ask [on StackOverflow](http://stackoverflow.com/questions/tagged/shake-build-system).

The one place many users might encounter the changes are that oracle rules now require a `type instance` defining between the `key` and `value` types. For example, if defining an oracle for the `CompilerVersion` given the `CompilerName`, you would have to add:

    type instance RuleResult CompilerName = CompilerVersion

As a result of this `type instance` the previously problematic `askOracle` can now infer the result type, removing possible sources of error and simplifying callers.

The redefining of rule types represents most of the work in this release.

**Add `cmd_`**

The `cmd_` function is not much code, but I suspect will turn out to be remarkably useful. The `cmd` function in Shake is variadic (can take multiple arguments) and polymorphic in the return type (you can run it in multiple monads with multiple results). However, because of the overloading, if you didn't use the result of `cmd` it couldn't be resolved, leading to ugly code such as `() <- cmd args`. With `cmd_` the result is constrained to be `m ()`, so `cmd_ args` can be used.

**Rework Skip/Rebuild**

Since the beginning Shake has tried to mirror the `make` command line flags. In terms of flags to selectively control rebuilding, `make` is based entirely on ordered comparison of timestamps, and flags such as `--assume-new` don't make a lot of sense for Shake. In this release Shake stops trying to pretend to be `make`, removing the old flags (that never worked properly) and adding `--skip` (don't build something even if it is otherwise required) and `--build` (build something regardless). Both these flags can take [file patterns](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#t:FilePattern), e.g, `--build=**/*.o` to rebuild all object files. I don't think these flags are finished with, but it's certainly less of a mess than before.
