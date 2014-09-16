# Towards Shake 1.0

_Summary: I've just released a new version of Shake, with a --demo feature and an underlying continuation monad. I want to release v1.0 in the near future._

I've just released a new version of the [Shake build system](https://github.com/ndmitchell/shake#readme), version 0.13.3. While the version number is only 0.0.1 higher, the [changelog](https://github.com/ndmitchell/shake/blob/master/CHANGES.txt) lists a large number of improvements. In particular, two changes are:

* The `Action` monad is now based on continuations, which allows Shake to suspend threads without requiring a GHC RTS thread. The result is significantly less memory used on thread stacks. I still find it quite amazing that Haskell has such powerful and robust abstraction mechanisms that a huge change doesn't even break the API.
* The `shake` binary now features a `--demo` mode, invoked by running `shake --demo`. This mode generates a Shake project, compiles it, and shows off some of the features of Shake. You can the output of `--demo` [here](https://github.com/ndmitchell/shake/blob/master/docs/Demo.md#readme).

**Version 1.0**

With the two features above, I'm now looking towards Shake version 1.0. I'm not looking to make any significant backwards-incompatible change, or necessarily any code/API changes at all. However, if you have anything you think should be addressed before reaching such a milestone, please [comment on the issue tracker](https://github.com/ndmitchell/shake/issues) or email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

**Shake website**

The one thing I still want to finish before releasing version 1.0 is to have a proper website for Shake. I've registered [shakebuild.com](http://shakebuild.com) which will host the content, and have set up GitHub pages to serve it up. I have some rough content in [the docs directory](https://github.com/ndmitchell/shake/tree/master/docs) and a prototype generator in [the website directory](https://github.com/ndmitchell/shake/tree/master/website) - as an example it currently generates something a bit like [this for the user manual](http://shakebuild.com/manual), but with a table of contents when run through the latest generator. I'd appreciate any help with the content, the generator, or the styling - just email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).
