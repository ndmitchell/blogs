# GHC: From Bug to Merge (2)

_Summary: The story of another bug, from report, patch, revisions, to merge._

I recently [posted the story](https://neilmitchell.blogspot.com/2018/12/ghc-from-bug-to-merge.html) of a GHC bug that took 3 months to fix, which isn't great. I hoped that the recent infrastructure work to [move GHC to GitLab](https://mail.haskell.org/pipermail/ghc-devs/2018-December/016613.html) would speed that up in future. Fortunately, I got to test that theory shortly after.

When experimenting with `RebindableSyntax` and `MonadFailDesugaring` I kept getting the error:

```
The failable pattern ‘Just x’
    is used together with -XRebindableSyntax. If this is intentional,
    compile with -Wno-missing-monadfail-instances.
```

It's annoying the warning is on by default, but nevermind, let's add `-Wno-missing-monadfail-instances` to silence the compiler. But alas, no flags could make the warning go away. Looking at [the code](https://gitlab.haskell.org/ghc/ghc/blob/e839ee2f91f9bcd390ead98e830b9e1d7d7b9240/compiler/typecheck/TcMatches.hs#L947-948
), it's clear why:

```
; if | rebindableSyntax && (desugarFlag || missingWarning)
        -> warnRebindableClash pat
```

If you have `RebindableSyntax` and  `MonadFailDesugaring` turned on, the value of the warning flag (`missingWarning`) is ignored. Boolean logic is fiddly, but replacing `||` with `&&` seems to do the right thing.

**Raise a PR**

At this point I got Shayne Fletcher involved, who actually ran with most of the steps from here onwards. Given the change is small and the original code was obviously untested, we decided to raise a [GitHub PR](https://github.com/ghc/ghc/pull/245), skipping the Trac ticket and GHC Proposal steps.

A few days later [GHC GitLab](https://gitlab.haskell.org/ghc) became available, so we closed the first PR and opened a [GitLab PR](https://gitlab.haskell.org/ghc/ghc/merge_requests/46).

**Fix the PR**

As with the previous bug to merge story, the immediate feedback was "please add a test suite entry", which we did.

Thanks to the better integration with CI etc, the PR clearly passed the tests and got merged shortly thereafter.

**Timeline**

24 Dec - raise GitHub PR
27 Dec - raise GitLab PR
27 Dec - request for changes
28 Dec - code updated
29 Dec - merged

This bug was small enough to skip the bug tracker and proposal process, but even ignoring those steps, the speed was fantastic even over the holiday period. Hopefully this speed is the new normal!
