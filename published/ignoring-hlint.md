# Ignoring HLint

_Summary: HLint now has more ways to ignore hints you don't like._

[HLint](https://github.com/ndmitchell/hlint#readme) makes suggestions about how to improve your Haskell code. But not everyone likes all the suggestions all the time, so HLint comes with ways of ignoring those annoying hints, and HLint 2.1.11 provides even more mechanisms. Without further ado, let's take a quick tour - full details are in the [HLint README](https://github.com/ndmitchell/hlint#readme).

**Method 1: the `--default` flag**

To ignore _all_ hints your code currently generates run `hlint` as normal, but passing the `--default` flag, which will generate a config file with all hints that fire set to ignore. Typically, when approaching a new code base to run HLint on, I start by doing:

```
hlint . --default > .hlint.yaml
```

After that, it's easy to remove ignored hints from `.hlint.yaml` one by one and fix the code.

**Method 2: Add `-ignore` directives**

In the `.hlint.yaml` file you can write:

```
- ignore: {name: Eta reduce}
```

This directive ignores the named hint, and is what `--default` generates. There are also more refined ways of ignoring a hint in certain modules, or ignoring all hints in certain modules (see the [README](https://github.com/ndmitchell/hlint#ignoring-hints)).

**Method 3: Add a `{- comment -}`**

Method 3 actually has 3 sub-methods, you can write any of:

* `{-# ANN module "HLint: ignore Eta reduce" #-}`
* `{-# HLINT ignore "Eta reduce" #-}`
* `{- HLINT ignore "Eta reduce" -}`

For `ANN` pragmas it is important to put them _after_ any `import` statements. If you have the `OverloadedStrings` extension enabled you will need to give an explicit type to the annotation, e.g. `{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}`. The `ANN` pragmas can also increase compile times or cause more recompilation than otherwise required, since they are evaluated by `TemplateHaskell`.

For `{-# HLINT #-}` pragmas GHC may give a warning about an unrecognised pragma, which can be supressed with `-Wno-unrecognised-pragmas`.

For `{- HLINT -}` comments they are likely to be treated as comments in syntax highlighting, which can lead to them being overlooked.

My current preference is `{- HLINT -}`, but I think GHC should just special case `{-# HLINT #-}` and then in a few GHC releases we could use that. Unfortunately, [other people disagree](http://haskell.1045720.n5.nabble.com/Treatment-of-unknown-pragmas-td5884484.html) with [me](http://haskell.1045720.n5.nabble.com/Treatment-of-unknown-pragmas-tp5884484p5884491.html), so `{- HLINT -}` is the best we have.

**Method 4: Using the C Pre Processor**

`hlint` defines the `__HLINT__` preprocessor definition (with value `1`), so problematic definitions (including those that don't parse) can be hidden with:

```haskell
#ifndef __HLINT__
foo = ( -- HLint would fail to parse this
#endif
```
