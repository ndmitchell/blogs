# GHC: From Bug to Merge

_Summary: The story of a bug, from report, proposal, patch, revisions, to merge._

Like all good bugs, this one starts with playing around, in this case seeing how possible it was to eliminate `String` from a `base`-like library. There are two interesting extensions:

* [`OverloadedStrings`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overloaded-string-literals) changes each string literal to be wrapped in `fromString`, so `"hello"` in the source code becomes `fromString "hello"` when desugared.
* [`RebindableSyntax`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax) changes so that when GHC sees `fromString` or `fail` (or lots of other functions) it uses the version in scope, rather than the version in the `base` library.

Taking a look at the code:

```haskell
foo x = do
    Just y <- x
    return y
```

In GHC 8.6 it desugars to something equivalent to:

```haskell
foo x = x >>= \v ->
    case v of
        Just y -> return y
        _ -> fail ("pattern match error" :: String)
```

In particular, it _does not_ insert a `fromString` around the generated `"pattern match error"`. If you are using the built-in `fail`, that's fine, since the built-in `fail` wants a `String`. But if we define a custom `fail` function which doesn't use `String`, but which takes the output of `fromString`, then we can get type errors with the definitions:

```haskell
newtype Text = Text String
fail (Text x) = error x
fromString = Text
```

The solution is fairly simple - desugar with an additional `fromString`. By following the process outlined in this post we've made that modification so GHC 8.8 will insert `fromString`.

**Raise a GHC ticket**

The first step after finding a bug is to raise a GHC ticket, which I did as [Trac 15645](https://ghc.haskell.org/trac/ghc/ticket/15645), also offering to fix it. After discussing a bit with the GHC developers, we came the conclusion that the essential property of the bug is:

> Overloaded strings should imply even generated strings are overloaded, if they are passed to user-controlled functions.

I think (and still think) that this ticket is a pure bug fix, partly because changing it does not imply changing the user manual in any way. However, GHC developers quite reasonably disagreed, and suggested I go through the GHC proposal process.

**Raise a GHC proposal**

The GHC Proposal process involves creating a proposal, having people discuss it, and then passing it via a committee. At this point I got Shayne Fletcher involved, who actually ran with most of the steps from here onwards. We raised [GHC Proposal 168](https://github.com/ghc-proposals/ghc-proposals/pull/168), setting out the case why the change should be made. We `@` mentioned people on GitHub, and posted to [Twitter](https://twitter.com/shayne_fletcher/status/1042396301361590272) to solicit opposing views, but only 4 people made any comment, and none of them disagreed. We submitted to the committee after the discussion was verifiably dead (not that it was ever super alive), and had the proposal accepted.

**Write the code**

After having the proposal accepted the hard work of writing the code began as [Phab D5251](https://phabricator.haskell.org/D5251). The code itself wasn't simple, but neither was it very complex. An initial version was reviewed with lots of good suggestions from Simon Peyton Jones. Shayne made some revisions, including adding a regression test, and the patch was ready. Sometime later the code was merged to GHC master.

**Timeline**

The whole process took a long time - 14 Sep to 12 Dec (fortunately all 2018), so nearly 3 months. That timeline was significantly extended because GHC is in the process of changing hosting for their code, which both makes it harder to merge stuff and involves the people who would normally be merging stuff doing other work. However, I thought it instructive to show where the time went.

* 14 Sep - 14 Sep: Open bug and discuss it
* 14 Sep - 17 Sep: Writing the proposal
* 17 Sep - 20 Sep: Discussing the proposal
* 20 Sep - 24 Sep: Confirming the discussion had died down
* 24 Sep - 21 Oct: Waiting for the committee decision
* 21 Oct - 22 Oct: Opening the Phab ticket
* 22 Oct - 25 Oct: Discussion on the code review
* 25 Oct - 29 Oct: Addressing review comments
* 29 Oct - 31 Oct: Additional review and discussion
* 1 Nov - 26 Nov: Waiting for people to give a final approval
* 26 Nov - 11 Dec: Waiting to merge in

I think the interesting thing is that of the 3 months, over 2 months was spent waiting for confirmations of a mechanical nature. However, every time real in-depth thoughtful discussion was required (code review, discussing the bug) it happened almost immediately.
