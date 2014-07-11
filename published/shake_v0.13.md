# Shake 0.13 is out

_Summary: Shake 0.13 is out, which contains a few API changes and several new features._

I've just released [Shake 0.13](http://hackage.haskell.org/package/shake). There are several new features, which I'll blog about in more detail over the next few weeks. If you're upgrading:

* `ShakeOptions` has additional fields, as it almost always does. Don't pattern match on this type directly, use record updates/selectors. The new member is `shakeChange` which lets you pick between basing file checking on modification time (the default), file digests, or combinations thereof.
* `shakeReport` is now `[FilePath]` instead of `Maybe FilePath`. You can now write multiple profiling reports, specify `-` to output a simplified report on `stdout`, or files ending with `.json` to generate JSON output.
*  Shake is replacing `**>` with `|*>` , `?>>` with `&?>` and `*>>` with `&*>` - although the old operators will be around for a few versions yet. The new operators are hopefully more memorable - they are either OR rules (`||`) which match build any one of several files, or AND rules (`&&`) which build multiple files simultaneously, on top of the standard `*>` and `?>` rules. 
* `defaultRule` is deprecated, and should be replaced with `priority 0 . rule`. The new priority mechanism allows defining rules at different priorities, which `*>` takes advantage of, so that now fully explicit matches take precedence over file-pattern matches.
* `Development.Shake.Sys` is gone and all `system` calls are now marked deprecated. Please use `cmd` or `command` instead.
* File times are recorded to higher precision, so files written in a fast loop are now likely to be detected as changing.
* The [Ninja emulation](https://github.com/ndmitchell/shake/blob/master/docs/Ninja.md#readme) now supports `-t compdb`, which is useful for CMake.

I don't expect these changes to hit many users, and all should be fairly localised tweaks.
