# Simple library: HTML

From Bake.

Discussions with Christopher Done


I tried playing around with Lucid, but decided not to go with it at
this point for a few reasons, one of which is that installing Lucid at
work is currently impossible due to temporary and rather unfortunate
issues (that make me want to do violence to a sysadmin). Instead, I
wrote my own mini-HTML templating library inside Bake, very much
inspired by your blog posts:
https://github.com/ndmitchell/bake/blob/master/src/General/HTML.hs

Like your blog post, the Monad is totally legit, there is consistency
with trailing _ for an attribute or tag which takes no attributes, and
two underscores for tags which do take attributes.

Unlike Lucid, I've got zero overloading, and it's based around String
not Text, and there are no attribute names as tag names (I have __ so
never actually need to overlap, I can just make style_ be the
attribute and style__ be the tag, with no style_ tag sugar). I think
the level of overloading would be hard to do with String, because the
list matching gets in the way. I have rather a lot of annoying str_
calls (turn a String into a HTML), but it's worked out quite nicely. I
appreciate mine is probably 100x slower than yours.

So thanks for the ideas behind Lucid, and I might think about
switching to Lucid itself further down the line.


I tried playing around with Lucid, but decided not to go with it at
this point for a few reasons, one of which is that installing Lucid
at work is currently impossible due to temporary and rather
unfortunate issues (that make me want to do violence to a
sysadmin). Instead, I wrote my own mini-HTML templating library
inside Bake, very much inspired by your blog posts:
https://github.com/ndmitchell/bake/blob/master/src/General/HTML.hs

Understandable. At work we were stuck on GHC 7.4 for a long time. I
think we’ve demonstrated it’s really easy to whip up a useful HTML lib
in a pretty trivial module.

Like your blog post, the Monad is totally legit, there is consistency
with trailing _ for an attribute or tag which takes no attributes, and
two underscores for tags which do take attributes.

Cool, I did a quick runover with the invariants fresh in my head and
everything looks good to me. I notice you’ve got combinators like
‘commas’ etc. I ended up with similar once in the past with
blaze. Nice to have stuff like that.

Unlike Lucid, I’ve got zero overloading, and it’s based around
String not Text, and there are no attribute names as tag names (I
have __ so never actually need to overlap, I can just make style_ be
the attribute and style__ be the tag, with no style_ tag sugar). I
think the level of overloading would be hard to do with String,
because the list matching gets in the way. I have rather a lot of
annoying str_ calls (turn a String into a HTML),

Right, Lucid does a silly amount of overloading. It seems you could at
least implement a simple IsString to get rid of the str_ calls,
perhaps?

class (a ~ ()) => IsString (HTML_ a) where fromString = str_
You need that liberal head and equality constraint so that it’ll
resolve the subtle case of "foo" >> ... because the type of "foo
will be HTML_ a. This constraint fixes that and lets it pick the
instance reliably. I was super happy about this.

So thanks for the ideas behind Lucid, and I might think about
switching to Lucid itself further down the line.

Welcome!

