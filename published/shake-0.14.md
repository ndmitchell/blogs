# Announcing Shake 0.14

_Summary: Shake 0.14 is now out. The `*>` operator became `%>`. If you are on Windows the `FilePath` operations work differently._

I'm pleased to announce Shake 0.14, which has one set of incompatible changes, one set of deprecations and some new features.

**The `*>` operator and friends are now deprecated**

The `*>` operator has been _the_ Shake operator for many years, but in GHC 7.10 the `*>` operator is exported by the Prelude, so causes name clashes. To fix that, I've added `%>` as an alias for `*>`. I expect to export `*>` for the foreseeable future, but you should use `%>` going forward, and will likely have to switch with GHC 7.10. All the Shake documentation has been updated. The `&*>` and `|*>` operators have been renamed `&%>` and `|%>` to remain consistent.

**Development.Shake.FilePath now exports System.FilePath**

Previously the module `Development.Shake.FilePath` mostly exported `System.FilePath.Posix` (even on Windows), along with some additional functions, but modified `normalise` to remove `/../` and made `</>` always call `normalise`. The reason is that if you pass non-normalised `FilePath` values to `need` Shake can end up with two names for one on-disk file and everything goes wrong, so it tried to avoid creating non-normalised paths.

As of 0.14 I now fully normalise the values inside `need`, so there is no requirement for the arguments to `need` to be normalised already. This change allows the simplification of directly exporting `System.FilePath` and only adding to it. If you are not using Windows, the changes are:

* `normalise` doesn't eliminate `/../`, but `normaliseEx` does.
* `</>` no longer calls `normalise`. It turned out calling `normalise` is pretty harmful for `FilePattern` values, which leads to fairly easy-to-make but hard-to-debug issues.

If you are using Windows, you'll notice all the operations now use `\` instead of `/`, and properly cope with Windows-specific aspects like drives. The function `toStandard` (convert all separators to `/`) might be required in a few places. The reasons for this change are discussed in [bug #193](https://github.com/ndmitchell/shake/issues/193).

**New features**

This release has lots of new features since 0.13. You can see a complete list in the [change log](https://github.com/ndmitchell/shake/blob/master/CHANGES.txt), but the most user-visible ones are:

* Add `getConfigKeys` to `Development.Shake.Config`.
* Add `withTempFile` and `withTempDir` for the `Action` monad.
* Add `-j` to run with one thread per processor.
* Make `|%>` matching with simple files much faster.
* Use far less threads, with corresponding less stack usage.
* Add `copyFileChanged`.

**Plans**

I'm hoping the next release will be 1.0!
