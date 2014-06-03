# Shake file hashes/digests

_Summary: Shake can now be configured to check file hashes/digests instead of modification times, which is great if you frequently switch git branches._

Build systems run actions on files, skipping the actions if the files have not changed. An important part of that process involves determining if a file has changed. The Make build system uses modification times to impose an ordering on files, but more modern build systems tend to use the modification time as a proxy for the file contents, where any change indicates the contents have changed (e.g. Shake, Ninja). The alternative approach is to compute a hash/digest of the file contents (e.g. SCons, Redo). As of version 0.13, Shake supports both methods, along with three combinations of them - in this post I'll go through the alternatives, and their advantages/disadvantages.

**Modification times** rely on the file-system updating a timestamp whenever the file contents are written. Modification time is cheap to query. Saving a file afresh will cause the modification time to change, even if the contents do not - as a result `touch` causes rebuilds. Unfortunately, working with git branches sometimes modifies a file but leaves it with the same contents, which can result in unnecessary rebuilds (see the bottom of this post for one problematic git workflow).

**File digests** are computed from the file contents, and accurately reflect if the file contents have changed. There is a remote risk that the file will change without its digest changing, but unless your build system users are actively hostile attackers, that is unlikely. The disadvantage of digests is that they are expensive to compute, requiring a full scan of the file. In particular, after every rule finishes it must scan the file it just built, and on startup the build system must scan all the files. Scanning all the files can cause empty rebuilds to take minutes. When using digests, Shake also records file sizes, since if a file size changes, we know the digest will not match - making most changed digests cheap to detect.

**Modification time and file digests** combines the two methods so that a file only rebuilds if both the modification time _and_ digest have changed. The advantage is that for files that have not changed the modification time will cheaply detect that, without ever computing the file hash. If the file has changed modification time, then a digest check may save an expensive rebuild, but even if it doesn't, the cost is likely to be small compared to rerunning the rule.

**Modification time and file digests on inputs** takes the previous method, but only computes digests for input files. Generated files (e.g. compiled binaries) tend to be large (expensive to compute digests) and not edited (rarely end up the same), so a poor candidate for digests. The file size check means this restriction is unlikely to make a difference when checking all files, but may have some limited impact when building.

**Modification time or file digests** combines the two methods so that a file rebuilds if _either_ modification time _or_ file digest have changed. I can't think of a sensible reason for using this setting, but maybe someone else can?

**Suggestions for Shake users**

All these options can be set with the `shakeChange` field of `shakeOptions`, or using command line flags such as `--digest` or `--digest-and-input`. Switching between some change modes will cause all files to rebuild, so I recommend finding a suitable mode and sticking to it.

* If you can't trust the modification times to change, use `ChangeDigest`.
* If you are using git and multiple branches, use `ChangeModtimeAndDigestInput`.
* If you have generated files that rewrite themselves but do not change, I recommend using `writeFileChanged` when generating the file, but otherwise use `ChangeModtimeAndDigest`.
* Otherwise, I currently recommend using `ChangeModtime`, but some users may prefer `ChangeModtimeAndDigest`.

**Appendix: The git anti-build-system workflow**

Certain common git workflows change files from the current version, to an old version, then back again - causing modification-time checks to run redundant rebuilds. As an example, imagine we have two branches `foo` and `bar`, based on remote branches `origin/foo` and `origin/bar`, both of which themselves are regularly synced to a common `origin/master` branch. The difference between `origin/foo` and `origin/bar` is likely to be small. To switch from an up-to-date `bar` to an up-to-date `foo` we can run `git checkout foo && git pull`. These commands switch to an out-of-date `foo`, then update it. As a result, any file that has changed since we last updated `foo` will change to an old version, then change to a new version, likely the same as it was before we started. This workflow requires build systems to support file digests.
