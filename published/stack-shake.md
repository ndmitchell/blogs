# Why did Stack stop using Shake?

_Summary: Stack originally used Shake. Now it doesn't. There are reasons for that._

The [Stack tool](http://docs.haskellstack.org/en/stable/README/) originally used [the Shake build system](http://shakebuild.com), as described [on the page about Stack's origins](https://github.com/commercialhaskell/stack/wiki/Stack's-origins). Recently [Edward Yang](http://ezyang.com/) asked [why doesn't Stack still use Shake](https://groups.google.com/d/msgid/haskell-stack/5c46f622-a261-487b-96fd-3c9a86edb4ce%40googlegroups.com?utm_medium=email&utm_source=footer) - a very interesting question. I've taken the information shared in that mailing list thread and written it up, complete with my comments and distortions/inferences.

Stack is all about building Haskell code, in ways that obey dependencies and perform minimal rebuilds. Already in Haskell the dependency story is somewhat muddied. GHC (as available through `ghc --make`) does advanced dependency tracking, including header includes and custom Template Haskell dependency directives. You can also run `ghc` in single-shot mode, compiling a file at a time, but the result is about 3x slower and GHC will still do some dependency tracking itself anyway. Layered on top of `ghc --make` is Cabal which is responsible for tracking dependencies with `.cabal` files, configured Cabal information and placing things into the GHC package database. Layered on top of that is Stack, which has multiple projects and needs to track information about which Stackage snapshot is active and shared build dependencies.

Shake is good at taking complex dependencies and hiding all the messy details. However, for Stack many of these messy details were the whole purpose of the project. When Michael Snoyman and Chris Done were originally writing Stack they didn't have much experience with Shake, and opted to go for simplicity and directly managing the pieces, which they viewed to be less risky.

Now that Stack is written, and works nicely, the question changes to if it is worth changing existing working code to make use of Shake. Interestingly, at the heart of Stack there is a "Shake-lite" - see [Control.Concurrent.Execute](https://github.com/commercialhaskell/stack/blob/master/src/Control/Concurrent/Execute.hs). This piece could certainly be replaced by Shake, but what would the benefit be? Looking at it with my Shake implementers hat on, there are a few things that spring to mind:

* This existing code is _O(n^2)_ in lots of places. For the size of Stack projects, compared to the time required to compile Haskell, that probably doesn't matter.

* Shake persists the dependencies, but the Stack code does not seem to. Would that be useful? Or is the information already persisted elsewhere? Would Shake persisting the information make `stack` builds which had nothing to do go faster? (The answer is almost certainly yes.)

* Since the code is only used on one project it probably isn't as well tested as Shake, which has a _lot_ of tests. On the other hand, it has a lot less features, so a lot less scope for bugs. 

* The code makes a lot of assumptions about the information fed to it. Shake doesn't make such assumptions, and thus invalid input is less likely to fail silently.

* Shake has a lot of advanced dependency forms such as [resources](http://neilmitchell.blogspot.co.uk/2013/02/summary-management-of-finite-resources.html). Stack currently blocks when simultaneous configures are tried, whereas Shake would schedule other tasks to run.

* Shake has features such as profiling that are not worth creating for a single project, but that when bundled in the library can be a useful free feature.

In some ways Stack as it stands avoids a lot of the best selling points about Shake:

* If you have lots of complex interdependencies, Shake lets you manage
them nicely. That's not really the case for Stack, but is in large
heterogeneous build systems, e.g. the GHC build system.

* If you are writing things quickly, Shake lets you manage
exceptions/retries/robustness quickly. For a project which has the
effort invested that Stack does, that's less important, but for things
like [MinGHC](https://github.com/fpco/minghc) (something Stack killed), it was critically important because no one cared enough to do all this nasty engineering.

* If you are experimenting, Shake provides a lot of pieces (resources,
parallelism, storage) that help explore the problem space without
having to do lots of work at each iteration. That might mean Shake is
more of a benefit at the start of a project than in a mature project.

If you are writing a version of Stack from scratch, I'd certainly recommend thinking about using Shake. I suspect it probably does make sense for Stack to switch to Shake eventually, to simplify ongoing maintenance, but there's no real hurry.
