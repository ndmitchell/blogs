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



# Leaking Space

A _space leak_ occurs when a garbage collected program uses more memory than the programmer expects. Space leaks are particularly common in lazy functional languages, such as Haskell, but also occur in other garbage collected languages, including Javascript, C# and Ruby. In this article we'll go through some example space leaks, discuss which language features lead to space leaks, how to spot space leaks, and how to eliminate them.

All languages supporting garbage collection follow a similar pattern - on collection pointers to memory are followed, and all memory that cannot be reached is freed. A space leak happens when the memory layout does not match the programmers intuition, when there is more reachable memory than expected.  Unsurprisingly, features that complicate the memory layout are particularly vulnerable to space leaks - the two examples this article focuses on are lazy-evaluation (where evaluation of an expression is delayed until its value is needed) and closures (a lambda expression combined with its environment). Both these features are found in lazy functional languages, such as Haskell.

### Example 1: A Simple Space Leak

Let's consider the following Haskell definition:

    xs = delete dead [alive, dead]

This fragment creates a variable `xs`. Using the `[_,_]` notation we create a 2 element list containing both `alive` and `dead`, then we remove the element `dead` from the list with `delete`. If we call `length xs` it will return 1, indicating there is only one element in `xs`. In the absence of lazy evaluation, the memory layout would be:

    xs = [alive]

Here `xs` points at a list containing `alive` as the only element, `dead` is not referenced, and thus can be garbage collected. But Haskell uses lazy evaluation (also known as call-by-need), so after defining `xs` the memory would actually appear as:

    xs = delete dead [alive, dead]

Instead of pointing at a value, `xs` points at an expression, which will be replaced with an evaluated version later. We can see that `xs` still has two references `dead`, thus `dead` cannot be garbage collected, even though we know that `dead` will never be used. The variable `dead` is part of a space leak.

Earlier, we mentioned that `length xs` will return 1, but as a consequence of computing the size of `xs`, it will have to evaluate `delete` to compute the length. The act of evaluating `length xs` forces `xs` to be evaluated, and eliminates the space leak. When using lists, we will end up with a space leak if we frequently add and delete elements, but never compute properties such as length or look up values.

More generally, a space leak can occur when the memory contains an expression waiting for further evaluation, where the expression grows regularly, but where the evaluated value would not grow. Such leaks are usually solved by forcing the evaluation, making evaluation of some binding _strict_ instead of lazy.  

#### Strictness Annotations

For the above example, we happen to know that `length` will force enough of the evaluation, but what about in other situations? Before we talk about how to force evaluation, we have to talk about how evaluated a binding already is:

* A binding is in **normal form** if it does not point at any values which will require further evaluation, including nested values. As an example, `[1,2]` is in normal form. Values which do not require further evaluation include literal numbers and constructors. Lists are formed the constructor `[]` for the empty list, and the constructor `(:)` (pronounced "cons") for the pair of an element and the rest of the list.
* A binding is in **weak head normal form** (WHNF) if the outermost value does not require further evaluation. For example `[1+2]` is in WHNF, but not normal form, since it can be reduced to `[3]`. All values in normal form are also in head normal form.
* A binding is a **thunk** if the outermost value can be evaluated, for example `1+2` is a thunk. A thunk is the opposite of WHNF.

To force a value to WHNF, Haskell provides _bang patterns_ (which often require the pragma `{-# LANGUAGE BangPatterns #-}` at the top of source files). We can define a function `f` which takes one argument as:

    f x = ...

Where `...` is the body of the function. The body of the function may or may not cause evaluation of the binding `x`. If we wish to force evaluation of `x` we can write:

    f !x = ...

Now we can guarantee that when evaluating the body of `f` the binding `x` will be in WHNF (in reality, the guarantee Haskell provides is somewhat weaker - but the approximation described here is sufficient as a rule of thumb). Using bang patterns we can define:

    seq !x y = y

The function `seq` is in the standard Haskell libraries, and is used to force the valuation of the first argument before returning the second. 

Taking the original example, we could write:

    seq (length xs) (print "I just evaluated xs")

This expression will first compute `length xs`, ensuring we evaluate `delete`, before printing a message. 

#### Forcing to Normal Form

We have been using `length xs` to force evaluation of the `delete` function, but `length` does not always reduce values to normal form. To evaluate `length` we only need to evaluate the spine of the list. For example, consider:

    xs = (1+2) : delete 1 [1,2]

This expression computes `(1+2)` and uses the cons operator `(:)` to add the value at the start of the list. After evaluating `length xs`, we will have the memory:

	xs = [1+2, 2]

The list is in WHNF, but is not in normal form. To force reduction to normal form we can instead evaluate `rnf xs` (where `rnf` comes from the `Control.DeepSeq` module in the `deepseq` package). This function ensures the result is in normal form.

#### Why Lazy?

Given that strictness avoids this space leak, and (as we will see later) several other space leaks, why not just make every value strict? Indeed, most languages do make all values strict, and there are even variations on Haskell which use strictness (cite Mu). At the same time, laziness does bring some benefits. Other papers discuss the advantages at length, but here are a few thoughts from this author:

* You can define your own control structures - syntactic forms like `if/then/else` can be defined on top of laziness, rather than being built it.
* You can separate the values from the execution path, allowing the compiler to compute the right things, which is much harder if you have advanced conditional expressions.
* Simulating laziness in a strict language is usually more effort than forcing strictness in a lazy language, so laziness can be a better default.
* When combining functions, a strict language will often require the simplistic composition to take more memory than a lazy language, as we'll see in the Example 2.  

### Example 2: Space leaks and optimisation

Let's take another example of a space leak. Consider the following code:

    sum [1..n]

In Haskell this expression creates the list containing the numbers 1 to `n`, then adds them up. In a strict language, this operation takes _O(n)_ space - it would first generate a list of length `n`, then call `sum`. However, in a lazy language the items in the list can be generated one at a time as they are needed by the `sum`, resulting in _O(1)_ space usage. Even if we replace `[1..n]` with numbers read from a file, we can still retain the _O(1)_ space usage by using laziness to automatically interleave reading numbers and computing the sum.

Unfortunately, the above code, when compiled with the usual Haskell compiler (GHC) takes _O(n)_ due to a space leak, but at `-O1` optimisation or above takes _O(1)_ space once more. More confusingly, for some definitions of `sum` the code takes _O(1)_ at all optimisation levels, and for other definitions the code always takes _O(n)_.

Let us consider why the space leak arises. Consider the following definition of `sum`:

    sum1 (x:xs) = x + sum1 xs
    sum1 [] = 0

The first equation says that if the list has at least one item in it, bind the first item to `x` and the list containing the remaining items to `xs`. The sum is then defined recursively by adding the first element to the sum of the remaining elements. The second equation expresses the base case, the sum of the empty list is 0. Let's consider evaluating `sum1 [1..n]` for some large value of `n`, which proceeds as follows:

    sum1 [1..n]                 -- initial value
    sum1 (1:[2..n])             -- sum1 requires the list 
    1 + sum1 [2..n]             -- sum1 reduces per the equation
    1 + sum1 (2:[3..n])         -- + requires both its arguments
    1 + (2 + sum1 [3..n])

You can follow the evaluation by looking at what the program will require next, working from the top-most left-most part of the expression. For example, initially `sum1` looks at the list to determine which expression to match, so we have to evaluate `[1..n]` to produce `1:[2..n]`. As evaluation proceeds we build up the term `1 + 2 + 3 + 4 ...`, taking `O(n)` space. While we never have the whole list in memory at once, we instead have all the items of the list joined with `+` operations.

Having identified the space leak, we can use strictness to eliminate it. Given the expression `1 + 2` we can reduce it to `3` immediately, and provided we keep performing the addition as the computation goes along, we will only use constant memory. Alas, with the definition of `sum1`, we actually have `1 + (2 + (3 ...`, meaning that `1` and `2` are only added at the very end. Fortunately addition is associative so we can redefine `sum` to build up `((1 + 2) + 3) ...`. We can define:

    sum2 xs = sum2' 0 xs
        where sum2' a (x:xs) = sum2' (a+x) xs
              sum2' a [] = a

We define `sum2` in terms of an auxiliary function `sum2'`, which takes an additional accumulator `a`, being the value of all elements of the list processed so far. Running this definition as before looks more promising:

    sum2 [1..n]
    sum2' 0 [1..n]
    sum2' 0 (1:[2..n])
    sum2' (0+1) [2..n] 
    sum2' (0+1) (2:[3..n]) 
    sum2' ((0+1)+2) [3..n]

Now we have literal numbers joined with addition, but the space leak is still present. Fortunately, there is now a suitable target for a strictness annotation. We can define:

    sum3 xs = sum3' 0 xs
        where sum3' !a (x:xs) = sum3' (a+x) xs
              sum3' !a [] = a

The strictness annotation on the accumulator argument `a` results in the accumulator being evaluated before the next element of the list is processed. Revisiting our trace, we see:

    sum3 [1..n]
    sum3' 0 [1..n]
    sum3' 0 (1:[2..n])
    sum3' (0+1) [2..n] 
    sum3' 1 [2..n] 
    sum3' 1 (2:[3..n]) 
    sum3' (1+2) [3..n]
    sum3' 3 [3..n]

The trace shows that `sum3` takes _O(1)_ space, and does not have a space leak. The default definition of `sum` in the standard Haskell libraries is defined equivalently to `sum2`. However, with optimisations turned on, the compiler can detect that the accumulator is used strictly. By examining `sum2` you should be able to convince yourself that if `sum2 xs` is ever evaluated at all, then as a necessary consequence, the accumulator `a` will have to be evaluated to produce a result, since it is the only resulting value from the function. Using this knowledge, and the knowledge that reordering operations is safe (since Haskell expressions do not have side-effects), the compiler adds a strictness annotation.

#### Higher-order functions

An experienced Haskell programmer may realise that the definitions of `sum` can all be defined using standard library functions: 

    sum1 = foldr (+) 0
    sum2 = foldl (+) 0
    sum3 = foldl' (+) 0

These definitions are all equivalent to the original versions using explicit recursion. These `fold` functions all _reduce_ the list, `foldr` using the `0` instead of the empty list and nesting the `(+)` to the right, while `foldl` starts at the left using `0` as an accumulator. The function `foldl'` is a version of `foldl` which also forces the accumulator to WHNF at each step.

Haskell programmers often prefer to define functions in terms of `fold` instead of explicit recursion. The Haskell Lint tool, called HLint (cite HLint), will even automatically suggest the `foldr/foldl` versions from the original recursive definitions.

The advantage of using canned recursion patterns is that the form of the recursion can be understood easily, and that general rules about aspects such as performance and space leaks can be formulated. For example, when using a reduction function which requires both its arguments to produce any result, such as `(+)`, only `foldl'` is capable of running in constant space, so is usually the right thing to do.

### Example 3: Space leak on mean

Let's take a look at another example:

    mean xs = sum xs `div` length xs

This function computes the `mean` of a list `xs` by taking the `sum` and dividing by the `length` (the backticks around `div` let us use a function as in infix operator). Assuming we use a space-leak-free definition of `sum`, how much space will `mean [1..n]` take?

Using lazy evaluation, namely reducing the top-most left-most expression first, the answer is `O(n)`. The reason is that to fully evaluate `sum xs` we must evaluate the entire list `xs`, but since that list is shared by `length xs`, the whole list `xs` will be retained in memory.

In this instance a smarter evaluation strategy could eliminate the space leak. If we evaluated the first element of `xs`, then applied both `sum` and `length` to it, we could execute in constant space. Another approach applicable for computing `mean [1..n]` would be to remove the sharing of the list:

	sum [1..n] `div` length [1..n]

Here we have duplicated the list twice, and both parts run in constant space, allowing the entire computation run in constant space. Unfortunately, there is a risk that the compiler might common-up the two definitions (a technique known as Common Subexpression Elimination or CSE), sharing the lists, and reintroducing the space leak. Indeed, the GHC compiler is careful about performing CSE partly to avoid introducing space leaks.

The solution is to take the pattern we used for `sum3` and extend it so instead of accumulating just the sum, we also accumulate the length. The full definition is: 

    mean xs = mean' 0 0 xs
        where mean' !sum !length (x:xs) = mean' (sum+x) (1+length) xs
              mean' !sum !length [] = sum `div` length  

We accumulate the `sum` and `length` as local parameters, which are marked strict arguments in the helper function. Earlier I mentioned that Haskell programmers frequently prefer higher-order functions as opposed to direct recursion, but the `foldl'` function introduced earlier only permits one accumulating argument. Fortunately Haskell provides pairs using the notation `(_,_)`, so we can pass one value consisting of both running accumulators.

    mean xs = foldl' f (0,0) xs
        where f (!sum,!length) x = (sum+x, length+x)

While the `foldll'` forces the accumulator to WNHF, we now have a tuple for the accumulator, so while the tuple is in WHNF, the values inside it will remain thunks. By adding the bang patterns inside the tuple match we can force these two values, ensuring the entire accumulator is in normal form and eliminating the space leak.

### Example 4: Strictness in the Garbage Collector

In all our previous examples we have manually inserted strictness annotations to eliminate the space leak. Unfortunately, Hughes (198_) showed that some space leaks cannot be removed by strictness evaluation, but fortunately Wadler (1987) showed how the garbage collector can be modified to remove them. Let's take a look at such an example. We can improve the quality of any academic paper by placing an exclamation at the end of the title, which we can implement as:

    improve xs = fst pair ++ "!" ++ snd pair
        where pair = firstLine xs 

    firstLine ('\n':ys) = ([], '\n':ys)
    firstLine (y:ys) = (y:fst rest, snd rest)
        where rest = firstLine ys

The `improve` function takes the source of the paper, and produces a new paper. It splits the text into a variable `pair` being a pair of the first line and the remaining text, using the auxiliary `firstLine`. The function then takes the first element of the pair using `fst`, and the second element using `snd` and uses the string append operator `++` to insert an exclamation mark character between them.  The first equation of `firstLine` matches strings with a leading newline character and produces an empty first line, followed by the text. The second equation recursively calls `firstLine` with everything but the first character, then creates a result where the first character is at the front of the first line. (The function will raise an error if called with a string containing no newline, adding an equation to handle `[]` is simple, and the reader is encouraged to think what such a modification would look like.)

It should be possible for `improve` to run in _O(1)_ space, producing an output character after examining each input character, and requiring only a small amount of memory. The reason for such behaviour stems from the second equation of `firstLine`, we can see that after matching `y:ys` (i.e. consuming an input character) we immediately produce `(y:_, _)`, making an output character available via lazy evaluation before making the recursive call. Unfortunately, using the obvious implementation technique, this function actually requires space proportional to the first line of `xs`, so _O(`fst pair`)_. To see why, let us consider what the evaluation of `improve "abc..."` produces:

    let rest4 = firstLine "..."
    let rest3 = ('c':fst rest4, snd rest4)
    let rest2 = ('b':fst rest3, snd rest3)
    let rest1 = ('a':fst rest2, snd rest2)
    'a':'b':'c':fst rest4 ++ "!\n" ++ snd rest1

In each step of `firstLine` we produce a pair where the second component of that pair is simply the second component of the recursive call. As a result, we end up with both a linear chain of `snd` calls and retain all the character data by referencing the first component of each `rest` variable. If we forced the `snd` functions then we would eliminate the space leak to produce:

    let rest4 = firstLine "..."
    'a':'b':'c':fst rest4 ++ "!\n" ++ snd rest4

Unfortunately, there is nowhere we could put a strictness annotation to perform the appropricate reduction. We want to force the evaluation of `snd`, but are also relying on the laziness of the pair in the recursive call to `firstLine` to acheive _O(1)_ space. Fortunately, the garbage collector can solve this problem for us. The function `snd` is a selector - given a pair, it selects the second component. It does not compute any new values, does not allocate memory, and is very cheap. As such, we can actually _evaluate `snd` during garbage collection_, which eliminates the space leak. The reduction of selector functions during garbage collection is now a standard feature of lazy functional languages, automatically removing space leaks that would be otherwise impossible to eliminate.

### Example 5: Space leaks and closures

All the examples so far have been in Haskell, but other garbage collected languages are also susceptible to space leaks. While few languages are lazy by default, many languages support _closures_ - a lambda expression or function, plus some variables bound in an environment. One popular language which makes extensive use of closures is Javascript. In Javascript we can write:

    function LoadAudio(url)
    {
        // Load 'url' into 'request.response'
        var request = new XMLHttpRequest();
        request.open('GET', url, false);
        request.responseType = 'arraybuffer';
        request.send();

        // Decode the audio data
        window.AudioContext = window.AudioContext || window.webkitAudioContext;
        var context = new AudioContext();
        context.decodeAudioData(request.response, function(audio) {
            document.getElementById("status").click = function(){
                alert(url + " is " + audio.duration + " seconds long");
            };
        });
    }

This function uses the `XMLHttpRequest` API to load a URL (such as an MP3 file), then uses the Web Audio API to decode the audio data. Once we obtain a decoded `audio` value, which implements the `AudioBuffer` interface, we then add an action when clicking on a `status` HTML element which tells the user how long the audio file lasts. After this function has run, the `status` element will have an `onclick` event which runs:

    alert(url + " is " + audio.duration + " seconds long");

This expression references the `audio` object, which stores all the audio data - taking at least as much memory as the original file. We only ever access the `duration` field, which is a number, taking a mere 8 bytes. As a result, we have a space leak.

This space leak follows much the same pattern as the lazy evaluation space leaks above. We are referencing an expression `audio.duration` which keeps alive a signficiant amount of memory, but when evaluated, use only a small amount of memory. As before, the solution is to force the evaluation sooner than necessary, which we can do with:

	var duration = audio.duration;
	document.getElementById("status").click = function(){
		alert(url + " is " + duration + " seconds long");
	};
    
#### Javascript Selectors

In fact, in this example, the garbage collector had sufficient information to know that such an evaluation is always beneficial (assuming the property simply returns an existing value, rather than allocating a new value). Since there are no other pointers to `audio` the value of `audio` is constant, and since `duration` is a read only property, we know it cannot change. This optimisation would be an instance of the selector optimisation in Example 4.

Unfortunately, the selector optimisation is less applicable in Javascript than in Haskell because by default all values are mutable mutable. As a simpler example, consider:

    var constants = {pi : 3.142, fiveDigitPrimes : [10007,10009,10037,...]};
    document.getElementById("fire").onclick = function(){
		alert(constants.pi);
    };

Here we define a dictionary containing both `pi` (a number) and `fiveDigitPrimes` (a long list), then add a handler that tells the user the value of `pi`. If `constants` was immutable, then the garbage collector could reduce `constants.pi` and remove the reference to `constants`. Alas, in Javascript, the user can write `constants = {pi : 3}` to mutate `constants`, or `constants.pi = 3` to mutate just the `pi` member inside, rendering such an optimisation unsafe.

While the difficulties of mutation mean that Javascript does not reduce such functions in practice, it is not an insurmountable barrier. Consider a memory layout where the garbage collector knew if each reference was being used in a read-only situation (i.e. `alert(constants.pi)`) or a read-write situation (i.e. `constants.pi = 3`). From this information it would be able to determine which variables were only used read-only and thus were guaranteed to be constant. If `constants` and `constants.pi` were both detected as read-only then the field lookup could be performed by the garbage collector, freeing the `fiveDigitPrimes`. If the dictionary implementation provided certain guarantees then even if only `constants` was constant the lookup would still be safe.

In Haskell lazy evaluation is common (the default) and space leaks due to selectors are unavoidable, making the choice of selector optimisation an easy decision. In languages like Javascript adding additional code to solve fixable space leaks at the cost of making the normal code slower may not always be a sensible trade-off.

### Detecting space leaks

It has been said that every non-trivial Haskell program probably has a space leak somewhere within it. I can well imagine they are right. Fortunately, we have heap profiling tools that help us detect things better than other languages.

Memory leaks are easy, you just run for a long period of time and see what is in memory - by definition, most things will be caused by the memory leak. Space leaks are harder since the memory usually goes away at the end.

We can profile.

Given the definition that a space leak is, as soon as we find it, it stops being a space leak.

<http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/>

<http://book.realworldhaskell.org/read/profiling-and-optimization.html>

### Are space leaks inevitable?

As garbage collection frees us from the monotony of manually managing memory, languages are free to add more advanced features. Indeed, garbage collection was first invented by John McCarthy to solve problems in Lisp, allowing more advanced language features. Of course, these advanced features make it harder to predict what memory is being used, resulting in space leaks.

Compilers for lazy functional languages have been grappling with space leaks for over thirty years and have developed an array of techniques for helping (CPS etc). Some of these may be directly applicable to other languages, others may inspire variants of them more suited.

While space leaks are worrisome, they are not fatal, and should be viewed as a trade-off - certain advanced language features lead to space leaks. Haskell has been used successfully in many projects (cite CUFP).

My hope for the future is that the tooling improves. A detected space leak is usually easy enough to solve, however, there are three hopes:

* In particular domains people will work to eliminate space leaks by construction, for example FRP and pipes/conduits/enumerators. Generally I hope people can come up with nice libraries that are correct by construction.

* Determine you have a space leak. This step is usually performed after a user reports a program using too much space, there are no practical tools for stating what the expecting space usage is, and for annotating methods that take "suspiciously high" space.

* The tooling could be improved so that once you knew you had a space leak it could be fixed. As an example, this author spent many days tracking down a space leak. Part of the problem is that you need robust tools, not just academic theory, and as Haskell has been an academic language, the tools have lagged behind. In addition, most authors occasionally track down a space leak, it is not a regular occurrence, so there is no particular itch to scratch.
