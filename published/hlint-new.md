# New HLint version and API features

_Summary: In the new version of HLint, errors are warnings and warnings are suggestions. I plan to remove the old API very soon._

I've just released a new version of [HLint](https://github.com/ndmitchell/hlint), the tool for suggesting hints to improve your Haskell code. This version comes with two big changes.

Firstly, hints have been reclassified in severity. What used to be errors are now warnings, and hints that used to be warnings are now suggestions. As people have mentioned in the past, nothing HLint suggested was _really_ an error, and now HLint matches that.

Secondly, there is now an `hlint` API entry point in `Language.Haskell.HLint3` which captures the pattern of running HLint with command-line arguments, but in process as a library. With that, I don't think there are any API patterns that are better captured by the `Language.Haskell.HLint` API. If no one contacts me with [issues](https://github.com/ndmitchell/hlint/issues), I will be making `Language.Haskell.HLint` a copy of `Language.Haskell.HLint3` in the next release.

This release (like all minor releases) fixes a few bugs and adds a few new features. As a few random examples, HLint now warns on a redundant `DefaultSignatures` language extension, suggests `fmap` in preference to `liftM` and warns when `otherwise` is used as a pattern variable. There is a complete [change log](https://github.com/ndmitchell/hlint/blob/master/CHANGES.txt) in the repo.
 