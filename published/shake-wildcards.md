# New Shake with better wildcard patterns

_Summary: The new version of Shake supports `**` patterns for directory wildcards._

I've just released [Shake 0.15.6](https://hackage.haskell.org/package/shake). Don't be mislead by the 0.0.1 increment of the release, it's got over 50 entries in the [changelog](http://hackage.haskell.org/package/shake-0.15.6/changelog) since the last release. There are quite a few bug fixes, documentation improvements and optimisations.

One of the most user visible features is the new wildcard patterns. In the previous version Shake supported `//` for matching any number of directories and `*` for matching within a path component, so to match all C source files in `src` you could write:

    src//*.c

In the new version of Shake you can also write:

    src/**/*.c

The `//` patterns remain supported, but I intend to encourage use of `**` in new code if these patterns don't end up having any unforeseen problems. The advantages of the patterns in the new version are:

* The `**` patterns seem to be the defacto standard nowadays, being used by [rsync](http://linux.die.net/man/1/rsync), [Ant](https://ant.apache.org/manual/dirtasks.html), [Gradle](https://docs.gradle.org/current/javadoc/org/gradle/api/tasks/util/PatternFilterable.html), [Jenkins](https://wiki.jenkins-ci.org/display/JENKINS/Workspace+Cleanup+Plugin) etc.
* People would often write `"src" </> "//*.c"`, which gives the unexpected result of `//*.c`. With `**` you aren't overloading directories at the same time so everything works out as expected.
* `**` patterns only match relative files, not absolute ones, which is what you usually want in a build system. If you want to match absolute files use `*/**`.
* The semantics of patterns were a bit confusing for things like `///` - I've now given them precise semantics, but `**` avoids this confusion.
* I've optimised the pattern matching for both flavours, so there is more precomputation and less backtracking (in practice I don't think that makes any difference).
* I've optimised directory traversal using a file pattern, so it doesn't list directories that can't possibly match, which gives a significant speedup.

For this release I've also improved the website at [shakebuild.com](http://shakebuild.com/) with more documentation - hopefully it is useful.
