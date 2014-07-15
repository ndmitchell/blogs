# Shake Rule Matching

Picks literals before wildcards. Alternatives. Priority. Can rewrite your code.

Picking a rule in Shake has to be pure, e.g. based purely on the
target name, not if files exist or not. That's important to ensure
that when rebuilding Shake never has to look at and resolve rules,
which makes rebuilding very fast. To get what you want, you can write:
    
    "*.tex" *> \out ->
         let tex_ =  tex -<.> "tex_"
         b <- doesFileExist tex_
         if b then copyFile tex_ src else do
            let org = tex -<.> "org"
            need [org]
            ...
            cmd ...

    priority 100 $ "ltxhdr.tex" *> ...
    priority 50 $ "*.tex" *> ...

Moreover, I suggest that all Shake built in rules get a priority of
somewhere between 0 and 1, and in particular ?> rules get priority of
0.5, while *> gets priority 0.5 if the path has a wildcard in it, but
1 if it doesn't. That way using explicit filenames overrides wildcards
(which is quite a reasonable thing to expect) and people who need more
control can get it explicitly. Of course, I'd discourage a heavy
reliance on priorities.

Shake already has two levels of rules (defaultRule and rule) and
internally uses priorities for rules, so I'm hoping that exposing
priorities will make things simpler and more direct - a nice cleanup
which also happens to make it easy to improve the defaults.

    (\x -> "*.tex" ?== x && takeFileName x /= "ltxhdr.tex") ?> ...

I think in my large Shake example 85% of rules are *>, 5% are |*>, and
10% are ?>. Most uses of ?> tend to be a sign of poor structuring,
since if it's hard to express to Shake how to build it, it's probably
also a bit confusing to the users.
