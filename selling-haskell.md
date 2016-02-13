# Selling Haskell in the pub

_Summary: A Haskell sales pitch without any code._

When trying to convince people in a pub (without easy access to illustrative code examples) to give Haskell a try, I use three main arguments:

**Low cost abstractions:** In Haskell the [cost of creating a helper function](eric lipert) is low, because the syntax for functions is very concise (a single line) and the optimiser often removes all the overhead from the function (by inlining it). The power of such helpers is greatly [enhanced by higher-order functions](joel on software). In many languages each function must be top-level (within a file or class), but defining local functions reused in a small block of code provides encapsulation and permits thinking [less about naming](naming is hard). In most languages there is a much higher cost per function, and thus trivial functions are insufficiently valuable. By reducing the cost, Haskell encourages less repetition and describing problems in a more abstract way, which is very helpful for taming program complexity.

**Refactoring works:** In Haskell, refactoring is easy, safe and regular. Most projects involve writing a chunk of code, then continually changing it as the project evolves. In other languages most refactorings have [complex side conditions](eric) that must be met. In Haskell, most refactorings are correct by construction, or if there are side conditions, code which requires those side-conditions is considered "evil" by the community. The static type checker ensures that most refactorings have been carried correctly. It is common to change a fundamental type in the middle of millions of lines of code, and quickly fix the changes required and have confidence that it works. The ability to refactor means that code can be evolve with the project, without [accumulating technical debt](someone).

**Language polygots:** There are few programmers who know _only_ Haskell. I know Haskell programmers who are also experts in Perl, PHP, C, Javascript, C++, Fortran, R, Algol - pretty much any language you care to name. In contrast, when my wife has attended other programming language meetups, many of the participants knew only that language. There are many reasons Haskell programmers know lots of languages, not least because Haskell has rarely been taught as a first language. I find it interesting that most people who are experts in Haskell and other languages typically prefer Haskell - I certainly do.

In response to these arguments, if people are starting to get convinced, they usually ask:

* Are there lots of libraries? [Yes, 6000+](hackage). There are more R libraries for statistics, more Perl libraries for regular expressions, etc - but most of the standard stuff is covered. Wrapping a C library with the [foreign function interface (FFI)](haskell ffi) isn't too hard.
* What's the performance like? It's usually somewhere between C and Python. If you carefully optimise you can get [close to the performance of C](language shootouts). The profiling tools are reasonable. Performance is usually not a problem, but can be solved with C and FFI if it is.
