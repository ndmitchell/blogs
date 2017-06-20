# HLint 2.0 - with YAML configuration

_Summary: I've just released HLint 2.0, which lets you configure the rules with a YAML file._

I've just released [HLint 2.0](https://github.com/ndmitchell/hlint) to [Hackage](https://hackage.haskell.org/). HLint has always been configurable by writing a specially crafted Haskell file (e.g. to ignore certain hints or add new ones). With HLint 2.0 we now support YAML configuration files, and moreover encourage them over the Haskell format.

**New Features of 2.0**

Perhaps the most useful feature of this version is that HLint will search the current directory upwards for a `.hlint.yaml` file containing configuration settings. If a project includes a `.hlint.yaml` in the root then you won't need to pass it on the command line, and there's a reasonable chance any editor integration will pick it up as well. This idea was shamelessly stolen from [Stylish Haskell](https://github.com/jaspervdj/stylish-haskell/).

HLint configuration files have also moved from Haskell syntax with special interpretation to YAML syntax. The Haskell syntax had become quite overloaded and was increasingly confused. The YAML syntax gives a fresh start with a chance to properly specify configuration directly rather than encoding it as Haskell expressions. The YAML configuration format enables a few features in this release, and going forward should enable more. To create a template `.hlint.yaml` file run `hlint --default > .hlint.yaml`. HLint continues to work without a configuration file.

In addition to a bunch of little hints, there is now a hint to suggest giving modules explicit export lists. I tend to always favour export lists, using `module Foo(module Foo) where` in the rare case they seem genuinely too much work. If you object to the hint, simply add to `.hlint.yaml`:

    - ignore: {name: Use module export list}

On the other extreme, if you always want to require a complete and explicit export list (banning the trick above) do:

    - warn: {name: Use explicit module export list}

Which enables the off-by-default hint requiring an explicit list. To see which off-by-default hints your program triggers pass the command line argument `--show`.

The biggest new hint is that you can now mark certain flags/extensions/imports/functions as being restricted - maintaining either a whitelist or blacklist and exceptions. As an example, HLint itself contains the restrictions:

    - functions:
      - {name: unsafeInterleaveIO, within: Parallel}
      - {name: unsafePerformIO, within: [Util.exitMessageImpure, Test.Util.ref]}
      - {name: unsafeCoerce, within: []}

These unsafe functions can only be used in the modules/functions listed (or nowhere for `unsafeCoerce`), ensuring no code sneaks in unexpectedly. As another example:

    - flags:
      - default: false
      - {name: [-fno-warn-incomplete-patterns, -fno-warn-overlapping-patterns]}

Here we have used `default: false` to say that any flags not explicitly allowed cannot be used. If someone accidentally checks in development code with `{-# OPTIONS_GHC -w #-}` then HLint will catch it. This restriction feature is particularly designed for [code reviews](http://neilmitchell.blogspot.co.uk/2017/04/code-review-reviewed.html).

**What Might Have Broken**

This release changes a lot of stuff. I am aware the following will no longer work:

* The `HLint2` API has been deleted - I don't think anyone was using it (the `HLint` and `HLint3` ones remain unchanged). I'll continue to evolve the API in future releases.
* The Haskell configuration file no longer has a way of importing other configuration files. At the same time I made it so the default configuration file and internal hints are always included, which I hopefully offsets most of the impact of ignoring hint imports.
* I may have broken some other things by accident, but that's why there is a [bug tracker](https://github.com/ndmitchell/hlint/issues).

At the moment the Haskell and YAML configuration files are both supported. However, I'll be phasing out the Haskell configuration in the future, and encourage people to move to the YAML. The conversion should be pretty simple.