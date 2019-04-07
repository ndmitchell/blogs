# Code Review: Approve with Suggestions

_Summary: Code review is not a yes/no decision - mostly I say yes with suggestions._

As I [wrote previously](https://neilmitchell.blogspot.com/2017/04/code-review-reviewed.html), I didn't used to be a fan of code review for `$WORK` code, but now I am. After I review some code there are three responses I might give:

* **"Request changes"** - this code has some fatal flaw. Maybe I saw a race condition. Maybe there's insufficient testing. Maybe it's just a bad idea. Please fix it or convince me I'm wrong and I'll review again.
* **"Approved"** - this code is great. Let's merge it. If the CI has already passed, I'll probably merge it now myself.
* **"Approved with suggestions"** - this code is fine, I'm happy for it to be merged, but I thought of a few ways to make it better.

I think I use "Approved with suggestions" about 80% of the time. To use this status I think the code is correct, readable, and will have no negative effects - I'm happy for it to be merged. At the same time, I can think of a few ways to improve it - e.g. using some utility function, simplifying things a bit, making the documentation clearer. If the original author disagrees with me, I'm not going to bother arguing. I'm happy for these commits to be in a follow up PR, or pushed on top of this PR, whatever suits them.

What's different about approved with suggestions, at least for trusted individuals (e.g. colleagues), is that if they make these tweaks I have no real interest in rereviewing. I'm happy for my approval to remain sticky and for them to seek out rereview only if they think they need it. Importantly, this guideline is consistent with my [reasons for reviewing](https://neilmitchell.blogspot.com/2017/04/code-review-reviewed.html). After the first review, if the code doesn't change meaningfully, a rereview offers none of the benefits that make me want to review in the first place.

Since the [programming language DAML](https://daml.com/) that I work on is [now open source](http://hub.digitalasset.com/press-release/digital-asset-open-sources-daml) I can point at a concrete example using [a pull request to our GHC fork](https://github.com/digital-asset/ghc/pull/6). Here [Shayne](http://blog.shaynefletcher.org/) added a function:

```
qualifyDesugar :: (String -> OccName) -> String -> RdrName
qualifyDesugar occName =
  (mkRdrQual $ mkModuleName "DA.Internal.Desugar") . occName
```

It matches the stated intention, but it seems to do a bit too much - it turns a `String` into an `OccName` using a supplied function, when it could have just taken the `OccName` directly. Simpler, better, more maintainable. So I suggested:

```
qualifyDesugar :: OccName -> RdrName
qualifyDesugar = mkRdrQual $ mkModuleName "DA.Internal.Desugar"
```

A nice improvement. However, Shayne is responsible enough to make such simple tweaks that it didn't require another review. A typical case of Approve with suggestions.
