# Solving Cabal Hell

Idea from Roman. Separate interfaces.

Thinking further, I still think that having shake-0.13 be both the
name of an interface and an implementation is a good thing. In many
cases shake-0.13 the package will implement shake-0.12 the interface
trivially (or just with a little more hiding). But in cases where it
doesn't, I think more of the power of Backpack specifications would be
great for writing the shake-0.12 interface on top of shake-0.13
interface. You could also imagine a shake-0.11 interface con top of
the shake-0.13 interface, then chain them up transitively. I also
think you'd want support for writing the files inline, not referring
to external files, since often these wrappers will be pretty short.

I totally agree about not writing interfaces and implementations
separately. My general suggestion was that the backwards compatible
"shortcut" everyone wants is to write a a package (e.g. shake-0.13)
and from that generate an interface (ishake-0.13) and an
implementation (shake-0.13 which implements the ishake-0.13
interface). If I had that, I'd almost certainly make shake-0.13
support the ishake-0.12 interface, and users would depend on an
explicit version, e.g. ishake-0.12, with the understanding that all
later versions of shake will almost certainly support that. You solve
Cabal hell and version range issues in one swoop and everyone uses
Backpack.

If we had that setup, I'd be very keen on regularly removing sugar
functions with no period of deprecation, and just include them in the
v0.12 to v0.13 stub. For shake-0.13 implementation of shake-0.12, it
might read:

package ishake-0.12-shake-0.13 where
    include shake-0.13
    include ishake-0.12
    Development.Shake.Sys = [import Development.Shake.Cmd]
    Development.Shake = [ (**>) = (&>); (*>>) = (|*>) ]
    Development.Shake.Rule = [defaultPriority = rule . priority 0.5]

So the diff between these is minimal - about 3 lines. If I had to put
each of those lines in it's own file I'd end up with 4 files to
express a 3 line diff, which is a bit sad. (In reality, shake-0.13
deprecated 2 of those, and removed one that got deprecated a few
versions back - this approach would have been far superior.)

Agreed. I think it's a useful feature, but of lower importance than
the auto-interface thing. Everyone gets bitten by version number
relationships, which the shake/ishake split solves for good. Some
people get bitten by interface subsetting, but it's only some users
(it has never hit me, for example). I can certainly see a desire to
write shake-big-0.13, and have ishake-0.13 be a subset of that - so I
can implement the union of the interfaces between ishake-0.13 and
ishake-0.12, and "remove" the functions I want to eliminate just at
the interface level - which is probably a particular use case of what
you are describing.
