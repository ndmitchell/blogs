# Turing Incomplete Languages

_Summary: Some languages ban recursion to ensure programs "terminate". That's technically true, but usually irrelevant._

In my career there have been three instances where I've worked on a programming language that went through the evolution:

1. Ban recursion and unbounded loops. Proclaim the language is "Turing incomplete" and that all programs terminate.
2. Declare that Turing incomplete programs are simpler. Have non-technical people conflate terminate quickly with terminate eventually.
3. Realise lacking recursion makes things incredibly clunky to express, turning simple problems into brain teasers.
4. Add recursion.
5. Realise that the everything is better.

Before I left university, this process would have sounded ridiculous. In fact, even after these steps happened _twice_ I was convinced it was the kind of thing that would never happen again. Now I've got three instances, it seems worth writing a blog post so for case number four I have something to refer to.

## A language without recursion or unbounded loops

First, let's consider a small simple statement-orientated first-order programming language. How might we write a non-terminating program? There are two easy ways. Firstly, write a loop - `while (true) {}`. Second, write recursion, `void f() { f() }`. We can ban both of those, leaving only bounded iteration of the form `for x in xs { .. }` or similar. Now the language is Turing incomplete and all programs terminate.

The lack of recursion makes programs harder to write, but we can always [use an explicit stack](https://learn1.open.ac.uk/mod/oublog/viewpost.php?post=162710) with unbounded loops.

The lack of unbounded loops isn't a problem provided we have an upper bound on how many steps our program might take. For example, we know [QuickSort](https://en.wikipedia.org/wiki/Quicksort) has worst-case complexity _O(n^2)_, so if we can write `for x in range(0, n^2) { .. }` then we'll have enough steps in our program such that we never reach the bound.

But what if our programming language doesn't even provide a `range` function? We can synthesise it by realising that in a linear amount of code we can produce exponentially large values. As an example:

```haskell
double xs = xs ++ xs -- Double the length of a list
repeated x = double (double (double (double (double (double (double (double (double (double [x])))))))))
```

The function `repeated 1` makes 10 calls to double, and creates a list of length 2^10 (1024). A mere 263 more calls to `double` and we'll have a list long enough to contain each atom in the universe. With some tweaks we can cause doubling to stop at a given bound, and generate numbers in sequence, giving us `range` to any bound we pick.

We now have a menu of three techniques that lets us write almost any program we want to do so:

1. We can encoding recursion using an explicit stack.
2. We can change unbounded loops into loops with a conservative upper bound.
3. We can generate structures of exponential size with a linear amount of code.

## The consequences

Firstly, we still don't have a Turing complete language. The code will terminate. But there is no guarantee on how long it will take to terminate. Programs that take a million years to finish technically terminate, but probably can't be run on an actual computer. For most of the domains I've seen Turing incompleteness raised, a runtime of seconds would be desirable. Turing incompleteness doesn't help at all.

Secondly, after encoding the program in a tortured mess of logic puzzles, the code is much harder to read. While there are three general purpose techniques to encode the logic, there are usually other considerations that cause each instance to be solved differently. I've written tree traversals, sorts and parsers in such restricted languages - the result is always a lot of comments and at least one level of unnecessary indirection.

Finally, code written in such a complex style often performs significantly worse. Consider QuickSort - the standard implementation takes _O(n^2)_ time worst case, but _O(n log n)_ time average case, and _O(log n)_ space (for the stack). If you take the approach of building an _O(n^2)_ list before you start to encode a `while` loop, you end up with _O(n^2)_ space and time. Moreover, while in normal QuickSort the time complexity is counting the number of cheap comparisons, in an encoded version the time complexity relates to allocations, which can be much more expensive as a constant factor.

## The solution

Most languages with the standard complement of `if`/`for` etc which are Turing incomplete do not gain any benefit from this restriction. One exception is in domains where you are proving properties or doing analysis, as two examples:

1. Dependently typed languages such as [Idris](https://www.idris-lang.org/), which typically have much more [sophisticated termination checkers](http://docs.idris-lang.org/en/latest/tutorial/theorems.html#totality-checking) than just banning recursion and unbounded loops.
2. Resource bounded languages such as [Hume](https://www.macs.hw.ac.uk/~greg/hume/), which allow better analysis and implementation techniques by restricting how expressive the language is.

Such languages tend to be a rarity in industry. In all the Turing incomplete programming languages I've experienced, recursion was later added, programs were simplified, and programming in the language became easier.

While most languages I've worked on made this evolution in private, one language, [DAML](https://daml.com) from [Digital Asset](https://www.digitalasset.com/), did so in public. [In 2016 they wrote](https://blog.digitalasset.com/blog/introducing-the-digital-asset-modeling-language-a-powerful-alternative-to-smart-contracts-for-financial-institutions):

> DAML was intentionally designed not to be Turing-complete. While Turing-complete languages can model any business domain, what they gain in flexibility they lose in analysability.

Whereas [in 2020 their user manual says](https://docs.daml.com/1.6.0/daml/intro/9_Functional101.html#recursion):

> If there is no explicit iterator, you can use recursion. Let’s try to write a function that reverses a list, for example.

Note that while I used to work at Digital Asset, these posts both predate and postdate my time there.
