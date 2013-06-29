# Space Leaks

### The brief

Queue is the ACM's magazine for practicing software engineers. Written by engineers for engineers, Queue focuses on the technical problems and challenges that loom ahead, helping readers to sharpen their own thinking and pursue innovative solutions. Queue does not focus on either industry news or the latest "solutions." Rather, Queue takes a critical look at current and emerging technologies, highlighting problems that are likely to arise and posing questions that software engineers should be thinking about. Typically about 5000 words.

In my discussion with the editorial board, the fact JavaScript and other languages have space leaks, and Haskell has a special problem with space-leaks came out.  The idea is that we present what the problem is *in Haskell* to a general audience, talk about remedies, and how space leaks both appear and can be addressed on other languages. If engineers come away with a "wow, I had no idea about spaceleaks; I'll look out for them" would be ideal. Haskell has great tools for tracking down space leaks because we have a problem with them!

Notes from meeting: "Arguably one of the biggest current problems with Haskell programming stems from a quirk having to do with reference holding that’s commonly referred to as “space leaks”. As a consequence, programs that semantically appear to have no reason to be consuming memory end up consuming memory just the same. So what’s being done to deal with this? And is there any cause to hope for a future remedy?"


### References

* <http://www.ittc.ku.edu/~neil/papers_and_talks/strictness.pdf> - space leaks and forcing evaluation with Haskell, Neil Sculthorpe, presentation.
* <http://www.haskell.org/haskellwiki/Memory_leak> - info about memory leaks generally, plus some good references to read - including the lines one where Simon Marlow talks about selector stuff.
* <http://blog.ezyang.com/2011/05/space-leak-zoo/> - classification of many types of space leak.
* <http://neilmitchell.blogspot.nl/2013/02/chasing-space-leak-in-shake.html> - my space leak blog post.
* <http://www.sciencedirect.com/science/article/pii/S1571066107005919> - plugging a space leak with an arrow, how people move away to forms of code which can't get space leaks.
* <http://onlinelibrary.wiley.com/doi/10.1002/spe.4380170904/abstract> - fixing some space leaks with a garbage collector, Wadler on how to modify the garbage collector.
* <http://dl.acm.org/citation.cfm?id=277719> - tail recursion and space efficiency.
* <https://www.bitba.se/i-accidentally-the-entire-heap/> - presentation on space leak


### Ideas

`sum` defined the obvious way has a space leak, which goes away with optimisation. Deleting items from a map leaves them in there until you "examine" the map.

Wadlers selector evaluation must be relevant.

What tools do we have to track down space leaks? Lag, drag, void and use?

Garbage collection answers memory allocation, finalisation is the answer to resource usage, but there is no answer to space leaks, other than space usage.

How much memory does this program take?

    sum [1..100]

Which sums the numbers from 1 to 100. In a strict language you would generate a list of 100 elements so _O(n)_. In a lazy language it runs in constant memory so _O(1)_. But it doesn't because of a space leak.

Strictness rescues us most of the time, but not always.


### Outline

Memory leaks and space leaks, using sum as the motivating example. What is a space leak?

# Leaking Space

A _space leak_ occurs when a garbage collected program uses more memory than the programmer expects. Space leaks are particularly common in lazy functional languages, such as Haskell, but also occur in other garbage collected languages, including Javascript, C# and Ruby. In this article we'll discuss which language features lead to space leaks, how to spot space leaks, and how to eliminate them.

All languages supporting garbage collection follow a similar pattern - on collection pointers to memory are followed, and all memory that cannot be reached is freed. A space leak happens when the memory does not match the programmers intuition, when there is more reachable memory than expected.  Unsurprisingly, features that complicate the memory layout are particularly vulnerable to space leaks - the two examples this article focuses on are lazy-evaluation (where evaluation of an expression is delayed until its value is needed) and closures (a lambda expression combined with its environment).

### A Simple Space Leak

Let's consider the following Haskell definition:

    myset = Set.delete dead (Set.fromList [alive, dead])

This fragment creates a variable `myset`. Using `Set.fromList` we create a 2 element set containing both `alive` and `dead`, then we remove the element `dead` from the set with `Set.delete`. If we call `Set.size myset` it will return 1, indicating there is only one element in the set. In the absence of lazy evaluation, the memory layout would be:

    myset := Set(alive)

Here `myset` points at a `Set` containing `alive` as the only element, and `dead` is not referenced (and can be garbage collected). But Haskell uses lazy evaluation (also known as call-by-need), so a more accurate model would be:

    myset := { Set.delete dead { Set.fromList [alive, dead] } }

We are using `{ }` to represent something that will be evaluated later (known as a thunk). We can see that `myset` still has two references `dead`, and `dead` cannot be garbage collected, even though we know that `dead` will never be used. We have a space leak.

Earlier, we mentioned that `Set.size myset` will return 1, but as a consequence of computing the size of `myset`, it will have to evaluate both `Set.delete` and `Set.fromList` to get at the underlying `Set`. The act of observing the size of the set will force `myset` to be computed and cause the space leak to disappear. In practice we would end up with a serious space leak if we frequently add/delete from the set, but never query the set.

More generally, a space leak usually occurs when the memory contains something waiting for further evaluation, and can be eliminated by forcing that evaluation. The process of forcing evaluation is known as making the evaluation strict.

#### Strictness Annotations

For a language that suffers with space leaks due to lazy evaluation, it is no surprise that we have lots of ways of denoting strictness. I talk about bang patterns as the primary mechanism:

    seq !a x = x

Note that if you write `v = seq (Set.size mysize) 1`, but then never evaluate `v`, nothing happens. In particular the pattern `v = seq a a` is meaningless.

The underlying method is `seq`, this function takes two arguments and evaluates the first one before returning the second one.

There are also bang patterns on functions, `evaluate` in the IO monad, and bang patterns on data.

Of course, given that strictness avoids this space leak, why not just be strict everywhere? Indeed, a strict variant of Haskell is a reasonable proposition (cite Mu), but has disadvantages. Plus it doesn't fix all strictness leaks.

### Space leaks and optimisation

Consider the following code:

    sum [1..n]

In Haskell this expression creates the list containing the numbers 1 to `n`, then adds them up. In a strict language, this operation takes _O(n)_ space. However, in a lazy language the items in the list can be generated one at a time as they are needed by the `sum`, resulting in _O(1)_ space usage.

Unfortunately, the above code, when compiled with the usual Haskell compiler (GHC) takes _O(n)_ due to a space leak, but at `-O1` optimisation or above takes _O(1)_ space once more. More confusingly, for some perfectly reasonable definitions of `sum` the code always takes _O(1)_, and for other definitions the code always takes _O(n)_.

Let us start by understanding why there is any potential for a space leak in the first place. Consider the following definition of sum:

    sum1 (x:xs) = x + sum1 xs
    sum1 [] = 0

You can read this code as if the list has at least one item bind `x` to the first item and `xs` to the list containing the remaining items. The sum is then defined by adding the first element to the sum of the remaining elements. As a base case, the sum of the empty list is 0. Let's consider evaluating `sum1 [1..n]` for some large value of `n`, it proceeds as follows:

    sum1 [1..n]                 -- initial value
    sum1 (1:[2..n])             -- sum1 requires the list 
    1 + sum1 [2..n]             -- sum1 reduces per the equation
    1 + sum1 (2:[3..n])         -- + requires both its arguments
    1 + (2 + sum1 [3..n])

You can follow the evaluation by looking at what the program will require next, working from the top-most left-most part of the expression. For example, initially `sum1` looks at the list to determine which expression to match, so we have to evaluate `[1..n]` to produce `1:[2..n]`. As evaluation proceeds we build up the term `1 + 2 + 3 + 4 ...`, taking up `O(n)` space in all cases. While we never have the whole list in memory at once, we do have all the items of the list.

Having identified the problem, we can take the tool of strictness to apply it. Given the expression `1 + 2` we can reduce it to `3` immediately, and provided we keep doing that as the computation goes along, we will only use constant memory. Alas, with the definition of `sum1`, we actually have `1 + (2 + (3 ...`, meaning that `1` and `2` are never added until the end. Fortunately `+` is associative so we can redefine `sum` to accumulate the number so we build up `((1 + 2) + 3) ...`. We can define:

    sum2 xs = sum2' 0 xs
        where sum2' a (x:xs) = sum2' (a+x) xs
              sum2' a [] = a

We define `sum2` in terms of an auxiliary function `sum2'`, which takes an additional accumulator, being the value of all elements of the list processed so far. Running this definition as before looks more promising:

    sum2 [1..n]
    sum2' 0 [1..n]
    sum2' 0 (1:[2..n])
    sum2' (0+1) [2..n] 
    sum2' (0+1) (2:[3..n]) 
    sum2' ((0+1)+2) [3..n]

Now we have fully evaluated numbers next to each other. There is still a space leak, since we build up the list of numbers, but we now have a suitable target for a strictness annotation. We can define:

    sum3 xs = sum3' 0 xs
        where sum3' !a (x:xs) = sum3' (a+x) xs
              sum3' !a [] = a

Now the strictness annotation on the accumulator argument `a` results in the accumulator being evaluated before the next element of the list is processed. Revisiting our trace, we see:

    sum3 [1..n]
    sum3' 0 [1..n]
    sum3' 0 (1:[2..n])
    sum3' (0+1) [2..n] 
    sum3' 1 [2..n] 
    sum3' 1 (2:[3..n]) 
    sum3' (1+2) [3..n]
    sum3' 3 [3..n]

Now we can see that `sum3` takes _O(1)_ space. The default definition of `sum` in the standard Haskell libraries is defined equivalently to `sum2`. However, at `-O1` and above, the compiler can detect the that the accumulator is strict. By examining `sum2` you should be able to convince yourself that if `sum2 xs` is ever evaluated at all, then as a necessary consequence, the accumulator `a` will have to be evaluated to produce a result, since it is the only resulting value from the function. Using this knowledge, and the knowledge that reordering operations is safe (since Haskell expressions do not have side-effects), the compiler adds in the strictness annotation.

#### Higher-order functions

An experienced Haskell programmer may realise that we can actually define:

    sum1 = foldr (+) 0
    sum2 = foldl (+) 0
    sum3 = foldl' (+) 0

These functions are exactly equivalent, but using higher-order functions. As a general rule-of-thumb if you are using `foldr` you should be producing _streaming_ data, e.g. data that can be lazily consumed - a typical example is producing a list from a list.

Most uses of `foldl` should probably be `foldl'`, certainly all instances where the accumulator is a small type, such as `Double` or `Int`.

### Space leaks with environment

The final illustrated example is from Wadler:

.....

### Space leaks and closures


### Javascript Environments



### Detecting space leaks

It has been said that every non-trivial Haskell program probably has a space leak somewhere within it. I can well imagine they are right. Fortunately, we have heap profiling tools that help us detect things better than other languages.

We can profile.

Given the definition that a space leak is, as soon as we find it, it stops being a space leak.

<http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/>

### Are space leaks inevitable?

As garbage collection frees us from the monotony of manually managing memory, languages are free to add more advanced features. Indeed, garbage collection was first invented by John McCarthy to solve problems in Lisp, allowing more advanced language features. Of course, these advanced features make it harder to predict what memory is being used, resulting in space leaks.

Compilers for lazy functional languages have been grappling with space leaks for over thirty years and have developed an array of techniques for helping (CPS etc). Some of these may be directly applicable to other languages, others may inspire variants of them more suited.

While space leaks are worrisome, they are not fatal, and should be viewed as a trade-off - certain advanced language features lead to space leaks. Haskell has been used successfully in many projects (cite CUFP).

My hope for the future is that the tooling improves. A detected space leak is usually easy enough to solve, however, there are three hopes:

* In particular domains people will work to eliminate space leaks by construction, for example FRP and pipes/conduits/enumerators. Generally I hope people can come up with nice libraries that are correct by construction.

* Determine you have a space leak. This step is usually performed after a user reports a program using too much space, there are no practical tools for stating what the expecting space usage is, and for annotating methods that take "suspiciously high" space.

* The tooling could be improved so that once you knew you had a space leak it could be fixed. As an example, this author spent many days tracking down a space leak. Part of the problem is that you need robust tools, not just academic theory, and as Haskell has been an academic language, the tools have lagged behind. In addition, most authors occasionally track down a space leak, it is not a regular occurrence, so there is no particular itch to scratch.
