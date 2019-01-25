# HLint "Unused LANGUAGE pragma" hints

_Summary: HLint detects unused language pragmas._

HLint has detected unused `LANGUAGE` pragmas for a while - where you enable an extension (e.g. `{-# LANGUAGE EmptyDataDecls #-}`) but don't use it. HLint v2.1.13 includes some improvements from [Yair](https://github.com/yairchu) and myself making these hints even more powerful. As a result, I thought it worth showing some examples of what HLint can do in this area. I started by running HLint on [all of Hackage](https://neilmitchell.blogspot.com/2018/11/downloading-all-of-hackage.html), which found 17,718 "Unused LANGUAGE pragma hints", including the examples in this post.

**Detecting unused extensions**

For extensions that show up as syntax (e.g. `EmptyDataDecls`, `ViewPatterns` etc), HLint [has rules](https://github.com/ndmitchell/hlint/blob/master/src/Hint/Extensions.hs) saying which constructs require which extensions. For extensions that aren't syntax directed (e.g. `AllowAmbiguousTypes` or `IncoherentInstances`), HLint can't detect whether they are used or not. In all, HLint has rules for detecting 36 different unused extensions. Taking a look at some examples from Hackage:

```
abcBridge-0.15\src\Data\ABC\AIG.hs:18:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE EmptyDataDecls #-}
Perhaps you should remove it.

mallard-0.6.1.1\lib\Database\Mallard\Validation.hs:4:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE TemplateHaskell #-}
Perhaps you should remove it.

scholdoc-texmath-0.1.0.1\src\Text\TeXMath\Writers\TeX.hs:1:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, GADTs #-}
Perhaps:
  {-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
```

As we can see, HLint can spot entirely redundant extension declarations, and also prune those that are partly redundant.

**Duplicate extensions**

Sometimes extension are simply duplicated, and HLint detects these, either between two separate pragmas, or within a single pragma.

```
ghcjs-base-stub-0.2.0.0\src\GHCJS\Marshal\Pure.hs:3:1: Warning: Use fewer LANGUAGE pragmas
Found:
  {-# LANGUAGE DefaultSignatures #-}
  {-# LANGUAGE DefaultSignatures #-}
Perhaps:
  {-# LANGUAGE DefaultSignatures #-}

abstract-deque-tests-0.3\Data\Concurrent\Deque\Tests.hs:1:1: Warning: Use fewer LANGUAGE pragmas
Found:
  {-# LANGUAGE BangPatterns, RankNTypes, CPP, BangPatterns #-}
Perhaps:
  {-# LANGUAGE BangPatterns, RankNTypes, CPP #-}
```

**Implied extensions**

The new feature for v2.1.13 is that extension are detected as redundant if they are implied by other extensions. For example, if you have `PolyKinds` defined then that implies `KindSignatures`. HLint now features [a list of such implications](https://github.com/ndmitchell/hlint/blob/master/src/HSE/Util.hs), which it uses to detect redundant extensions.

```
AERN-RnToRm-0.5.0.1\src\Data\Number\ER\RnToRm\UnitDom\Base.hs:1:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE MultiParamTypeClasses #-}
Perhaps you should remove it.
Note: Extension MultiParamTypeClasses is implied by FunctionalDependencies

attoparsec-0.13.2.2\Data\Attoparsec\ByteString\Char8.hs:1:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE BangPatterns, CPP, FlexibleInstances, TypeFamilies,
    TypeSynonymInstances, GADTs #-}
Perhaps:
  {-# LANGUAGE BangPatterns, CPP, FlexibleInstances, TypeFamilies,
    GADTs #-}
Note: Extension TypeSynonymInstances is implied by FlexibleInstances
```

**Redundant extensions that imply non-redundant extensions**

Sometimes there is an extension that you can tell is unused (e.g. `RecordWildCards`), which implies an extension that is either being used or can't be detected (e.g. `DisambiguateRecordFields`). In such cases HLint gives a note that the implied extension might now need to be provided explicitly, although usually it won't be necessary. As examples:

```
gogol-maps-engine-0.3.0\gen\Network\Google\Resource\MapsEngine\Projects\List.hs:7:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE RecordWildCards #-}
Perhaps you should remove it.
Note: may require `{-# LANGUAGE DisambiguateRecordFields #-}` adding to the top of the file

manifolds-0.5.0.1\Data\Function\Affine.hs:14:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE FunctionalDependencies #-}
Perhaps you should remove it.
Note: may require `{-# LANGUAGE MultiParamTypeClasses #-}` adding to the top of the file
```

**Being wrong**

Finally, sometimes HLint gets it a bit wrong. As an example:

```
shake-0.17.1\src\Development\Shake\Internal\FileInfo.hs:1:1: Warning: Unused LANGUAGE pragma
Found:
  {-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP,
    ForeignFunctionInterface #-}
Perhaps:
  {-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, CPP
    #-}
```

Here HLint has detected that `ForeignFunctionInterface` is not used, but in fact it is, although only under one branch of an `#ifdef`. In this case it would have been possible to put the extension itself under CPP, or to adjust the CPP definitions given to HLint, or to just [ignore the hint](https://neilmitchell.blogspot.com/2019/01/ignoring-hlint.html).
