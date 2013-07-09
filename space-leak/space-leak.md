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

This fragment creates a variable `xs`. We create a two element list using the `[_,_]` notation, containing both `alive` and `dead`, then we remove the element `dead` from the list with `delete`. If we call `length xs` it will return 1, indicating there is only one element in `xs`. In the absence of lazy evaluation, the memory layout would be:

    xs = [alive]

Here `xs` references a list containing `alive` as the only element, `dead` is not referenced, and thus can be garbage collected. But Haskell uses lazy evaluation (also known as call-by-need), so after defining `xs` the memory would actually appear as:

    xs = delete dead [alive, dead]

Instead of pointing at a _value_, `xs` points at an _expression_, which may be replaced with an actual value later. We can see that `xs` still has two references to `dead`, thus `dead` cannot be garbage collected, even though we know that `dead` will never be used. The variable `dead` is part of a space leak.

Earlier we mentioned that `length xs` will return 1, but as a consequence of computing the length, it has to evaluate `delete`. The act of evaluating `length xs` reduces `xs` to a value, which eliminates the space leak. When using lists, we can end up with a space leak if we frequently add and delete elements, but never compute properties such as length or look up values.

More generally, a space leak can occur when the memory contains an expression, where the expression grows regularly, but where the evaluated value would not grow. Such leaks are usually solved by forcing the evaluation, making evaluation of some binding _strict_ instead of lazy.  

#### Strictness Annotations

In the above example, we happen to know that `length` will force enough of the evaluation, but `length` is not always the answer. Before we talk about how to force evaluation, we have to talk about how evaluated a binding already is:

* A binding is in **normal form** if it does not point at any values which will require further evaluation, including nested values. As an example, `[1,2]` is in normal form. Values which do not require further evaluation include literal numbers and constructors. Lists are formed the constructor `[]` for the empty list, and the constructor `(:)` (pronounced "cons") for the pair of an element and the rest of the list.
* A binding is in **weak head normal form** (WHNF) if the outermost value does not require further evaluation. For example `[1+2]` is in WHNF, but not normal form, since it can be reduced to `[3]`. All values in normal form are also in WHNFs.

To force a value to WHNF, Haskell provides _bang patterns_ (which often require the pragma `{-# LANGUAGE BangPatterns #-}` at the top of source files). We can define a function `f` which takes one argument as:

    f x = ...

Where `...` is the body of the function. The body of the function may or may not cause evaluation of the binding `x`. If we wish to force evaluation of `x` we can write:

    f !x = ...

Now we can guarantee that when evaluating the body of `f` the binding `x` will be in WHNF (in reality, the guarantee Haskell provides is somewhat weaker - but the approximation described here is sufficient as a rule of thumb). Using bang patterns we can define:

    seq !x y = y

The function `seq` is in the standard Haskell libraries, and is used to force the valuation of the first argument before returning the second. 

Taking the original example, we could write:

    seq (length xs) (print "I just evaluated xs")

This expression will first compute `length xs`, ensuring we evaluate `delete`, before printing the message. 

#### Forcing to Normal Form

We have been using `length xs` to force evaluation of the `delete` function, but `length` does not always reduce values to normal form. To evaluate `length` we only need to evaluate the spine of the list. For example, consider:

    xs = (1+2) : delete 1 [1,2]

This expression computes `(1+2)` and uses the cons constructor `(:)` to join the value at the start of the list. After evaluating `length xs`, the memory will be:

	xs = [1+2, 2]

The variable `xs` is in WHNF, but not in normal form. To force reduction to normal form we can evaluate `rnf xs` to WHNF (where `rnf` comes from the `Control.DeepSeq` module in the `deepseq` package), which ensures `xs` is in normal form:

    seq (rnf xs) (print "I just evaluated xs")

#### Why Lazy?

Given that strictness avoids this space leak, and (as we will see later) several other space leaks, why not make all values strict? Certainly most languages have strict values, and there are even variants of Haskell that default to strict evaluation (cite Mu). As with all language design decisions, lazy evaluation is a design trade-off - space leaks are a disadvantage - but there are many other advantages. Other papers discuss the advantages of lazy evaluation at length, but here are a few reasons:

* Simulating laziness in a strict language is usually more effort than forcing strictness in a lazy language, so laziness can be a better default.
* Defining new control structures in strict languages often requires macros or building them into the compiler, while lazy evaluation allows such patterns to be expressed directly.
* Laziness allows variable bindings to be made without considering which bindings are evaluated where, a great simplification when combined with complex conditionals.
* When combining functions, a strict language will often require the simplistic composition to take more memory than a lazy language, as we'll see in the next example.

### Example 2: Space leaks and optimisation

Let's take another example of a space leak. Consider the following code:

    sum [1..n]

In Haskell this expression creates the list containing the numbers 1 to `n`, then adds them up. In a strict language, this operation takes _O(n)_ space - it would first generate a list of length `n`, then call `sum`. However, in a lazy language the items in the list can be generated one at a time as they are needed by the `sum`, resulting in _O(1)_ space usage. Even if we replace `[1..n]` with numbers read from a file, we can still retain the _O(1)_ space usage by using laziness to automatically interleave reading numbers and computing the sum.

Unfortunately, the above code, when compiled with the Glasgow Haskell Compiler (GHC) takes _O(n)_ due to a space leak, but at `-O1` optimisation or above takes _O(1)_ space. More confusingly, for some definitions of `sum` the code takes _O(1)_ at all optimisation levels, and for other definitions the code always takes _O(n)_.

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

Having identified the space leak, we can use strictness to eliminate it. Given the expression `1 + 2` we can reduce it to `3` immediately, and provided we keep performing the addition as the computation goes along, we will only use constant memory. Alas, with the definition of `sum1`, we actually have `1 + (2 + (3 ...`, meaning that `1` and `2` cannot be reduced. Fortunately addition is associative, so we can redefine `sum` to build up `((1 + 2) + 3) ...`. We can define:

    sum2 xs = sum2' 0 xs
        where
            sum2' a (x:xs) = sum2' (a+x) xs
            sum2' a [] = a

We define `sum2` in terms of an auxiliary function `sum2'`, which takes an additional accumulator `a`, being the value of all elements of the list processed so far. Tracing the evaluation looks more promising:

    sum2 [1..n]
    sum2' 0 [1..n]
    sum2' 0 (1:[2..n])
    sum2' (0+1) [2..n] 
    sum2' (0+1) (2:[3..n]) 
    sum2' ((0+1)+2) [3..n]

Now we have literal numbers joined with addition, but the space leak is still present. Fortunately, there is now a suitable target for a strictness annotation. We can define:

    sum3 xs = sum3' 0 xs
        where
            sum3' !a (x:xs) = sum3' (a+x) xs
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

An experienced Haskell programmer may realise that our definitions of `sum` can be defined in terms of standard library functions: 

    sum1 = foldr (+) 0
    sum2 = foldl (+) 0
    sum3 = foldl' (+) 0

These definitions are equivalent to our original versions using explicit recursion. The `fold` functions all _reduce_ the list, `foldr` using `0` instead of the empty list and nesting the `(+)` to the right, while `foldl` starts at the left using `0` as an accumulator. The function `foldl'` is a version of `foldl` which also forces the accumulator to WHNF at each step.

Haskell programmers often prefer to define functions in terms of `fold` instead of explicit recursion. The Haskell Lint tool, called HLint (cite HLint), will even automatically suggest the `foldr/foldl` versions from the original recursive definitions.

The advantage of using canned recursion patterns is that the form of the recursion can be understood easily, and general rules about performance and space leaks can be formulated. For example, when using a reduction function which requires both its arguments to produce any result, such as `(+)`, only `foldl'` is capable of running in constant space, so is usually the right choice.

You may wonder why Haskell does not use `sum` based on `foldl'` by default. In Haskell the `(+)` function can be overloaded, and some versions may not require both arguments to produce a result, so `foldl'` would be unnecessarily strict.

### Example 3: Space leak on mean

Let's take a look at another example:

    mean xs = sum xs `div` length xs

This function computes the `mean` of a list `xs` by taking the `sum` and dividing by the `length` (the backticks around `div` let us use a function as in infix operator). Assuming we use a space-leak-free definition of `sum`, how much space will `mean [1..n]` take?

Using lazy evaluation, namely reducing the top-most left-most expression first, the answer is `O(n)`. To fully evaluate `sum xs` we must evaluate the entire list `xs`, but since that list is used by `length xs`, the whole list `xs` will be retained in memory instead of being collected as it is produced.

In this example a smarter evaluation strategy could eliminate the space leak. If we evaluated the first element of `xs`, then applied both `sum` and `length` to it, the function would take constant space. Another approach applicable for computing `mean [1..n]` would be to remove the sharing of the list:

	sum [1..n] `div` length [1..n]

Here we have duplicated the list twice, and both parts run in constant space, allowing the entire computation run in constant space. Unfortunately, there is a risk that the compiler might common-up the two definitions (a technique known as Common Subexpression Elimination or CSE), sharing the lists, and reintroducing the space leak. Additionally, if the lists take effort to compute that effort will also be duplicated.

The solution is to take the pattern we used for `sum3` and extend it so instead of accumulating just the sum, we also accumulate the length. The full definition is: 

    mean xs = mean' 0 0 xs
        where
            mean' !s !l (x:xs) = mean' (s+x) (1+l) xs
            mean' !s !l [] = s `div` l

We accumulate the sum (`s`) and length (`l`) as local parameters, which are marked strict arguments in the helper function. The resulting definition runs in _O(1)_ and has no space leak. Earlier I mentioned that Haskell programmers frequently prefer higher-order functions to direct recursion, but the `foldl'` function introduced earlier only permits one accumulating argument. The solution is to join both accumulators into a single accumulator using the pair notation `(_,_)`.

    mean xs = foldl' f (0,0) xs
        where
            f (!sum,!length) x = (sum+x, length+x)

While the `foldl'` forces the accumulator to WNHF, we now have a pair as the accumulator, so while the pair is in WHNF, the values inside it remain unevaluated expressions. By adding the bang patterns inside the pair match we can force these two values, ensuring the entire accumulator is in normal form and eliminating the space leak.

### Example 4: Strictness in the Garbage Collector

In the previous examples we have inserted strictness annotations to eliminate space leaks. However, not all space leaks can be removed by strictness annotations (Huges 1980), sometimes we require special behavior from the garbage collector (Wadler 1987). As an example, let's improve the impact of an academic paper by placing an exclamation at the end of the title, which we can implement with:

    improve xs = fst pair ++ "!" ++ snd pair
        where pair = firstLine xs 

    firstLine ('\n':ys) = ([], '\n':ys)
    firstLine (y:ys) = (y:fst rest, snd rest)
        where rest = firstLine ys

The `improve` function takes the source of the paper, and produces a new paper. It splits the text into a variable `pair` being a pair of the first line and the remaining text, using the auxiliary `firstLine`. The function then takes the first element of the pair using `fst`, and the second element using `snd` and uses the string append operator `++` to insert an exclamation mark character between them.  The first equation of `firstLine` matches strings with a leading newline character and produces an empty first line, followed by the text. The second equation recursively calls `firstLine` with everything but the first character, then creates a result where the first character is at the front of the first line. (The function will raise an error if called with a string containing no newline, adding an equation to handle `[]` is simple, and the reader is encouraged to think what such a modification would look like.)

It should be possible for `improve` to run in _O(1)_ space, producing an output character after examining each input character, and requiring only a small amount of memory. Looking at the second equation of `firstLine` we see that after matching `y:ys` (i.e. consuming an input character) we immediately produce `(y:_, _)`, making an output character available via lazy evaluation before making the recursive call. Unfortunately, using the obvious implementation techniques, this function requires space proportional to the first line of `xs`, so _O(`fst pair`)_. To see why, let us consider the evaluation of `improve "abc..."`:

    let rest4 = firstLine "..."
    let rest3 = ('c':fst rest4, snd rest4)
    let rest2 = ('b':fst rest3, snd rest3)
    let rest1 = ('a':fst rest2, snd rest2)
    'a':'b':'c':fst rest4 ++ "!" ++ snd rest1

In each step of `firstLine` we produce a pair where the second component of that pair is simply the second component of the recursive call. As a result, we end up with both a linear chain of `snd` calls and retain all the character data by retaining a reference to the first component of each `rest` variable. If we forced the `snd` functions we would eliminate the space leak and produce:

    let rest4 = firstLine "..."
    'a':'b':'c':fst rest4 ++ "!\n" ++ snd rest4

Unfortunately, there is nowhere we could put a strictness annotation to perform the appropriate reduction. We want to force the evaluation of `snd`, but are also relying on the laziness of the pair in the recursive call of `firstLine` to acheive _O(1)_ space. Fortunately, the garbage collector can solve this problem for us. The function `snd` is a selector - given a pair, it selects the second component. It does not compute any new values, does not allocate memory, and is very cheap. As such, we can actually _evaluate `snd` during garbage collection_, which eliminates the space leak. The reduction of selector functions during garbage collection is now a standard feature of lazy functional languages, automatically removing space leaks that would be otherwise impossible to eliminate.

### Example 5: Space leaks and closures

All the examples so far have been in Haskell, but other garbage collected languages are also susceptible to space leaks. While few languages are lazy by default, many languages support _closures_ - a lambda expression or function, plus some variables bound in an environment. One popular language which makes extensive use of closures is Javascript.

Let's use the new Web Audio API to retrieve an MP3 file and compute its duration. In Javascript we can write:

    function LoadAudio(mp3)
    {
        // Load 'url' into 'request.response'
        var request = new XMLHttpRequest();
        request.open('GET', mp3);
        request.responseType = 'arraybuffer';

        request.onreadystatechange = function(){
            if (request.readyState != 4) return;

            // Decode the audio data
            window.AudioContext = window.AudioContext || window.webkitAudioContext;
            var context = new AudioContext();
            context.decodeAudioData(request.response, function(audio){
                document.getElementById("status").onclick = function(){
                    alert("MP3 is " + audio.duration + " seconds long");
                }
            });
        };
        request.send();
    }

This function uses the `XMLHttpRequest` API to load an MP3 file, then uses the Web Audio API to decode the audio data. Using the decoded `audio` value we add an action which tells the user the MP3's duration whenever a `status` button is clicked.

The implementation uses three local functions, two of which reference variables defined locally to `LoadAudio`, and those variables will be captured inside a closure when the local functions are created. As an example, the first function which is assigned to `onreadystatechange` captures the `request` variable defined three lines before.

After `LoadAudio` has run, the `status` button will have an `onclick` event which will run the following code:

    alert("MP3 is " + audio.duration + " seconds long");

This code references the `audio` object, which stores the audio data - taking at least as much memory as the original MP3. In practice, we only ever access the `duration` field, which is a number, taking a mere 8 bytes. As a result, we have a space leak.

This space leak has many aspects in common with the lazy evaluation space leaks above. We are referencing an expression `audio.duration` which keeps alive a significant amount of memory, but when evaluated, uses only a small amount of memory. As before, the solution is to force the evaluation sooner than necessary:

	var duration = audio.duration;
	document.getElementById("status").onclick = function(){
		alert("MP3 is " + duration + " seconds long");
	};

Now we compute the duration before registering the `onclick` event, and no longer reference the `audio` element, allowing it to be garbage collected.

#### Javascript Selectors

While we can modify the code to eliminate the space leak, could the garbage collector have eliminated the space leak for us? The answer is yes,  provided that `audio.duration` is cheap to compute, cannot not change in future and will not cause any side effects. Since there are no other references to `audio` the value `audio` refers to cannot change, and since `audio.duration` is a read only field it was likely allocated when the `audio` value was constructed. This optimisation would be an instance of the selector optimisation in Example 4.

Unfortunately, the selector optimisation is less applicable in Javascript than in Haskell, because most values are mutable. As a simpler example, consider:

    var constants = {pi : 3.142, fiveDigitPrimes : [10007,10009,10037,...]};
    document.getElementById("fire").onclick = function(){
		alert(constants.pi);
    };

Here we define a dictionary containing both `pi` (a number) and `fiveDigitPrimes` (a large array), then add an event handler that uses only `pi`. If `constants` was immutable, then the garbage collector could reduce `constants.pi` and remove the reference to `constants`. Alas, in Javascript, the user can write `constants = {pi : 3}` to mutate `constants`, or `constants.pi = 3` to mutate just the `pi` member inside, rendering such an optimisation unsafe.

While the difficulties of mutation mean that Javascript does not reduce such functions in practice, it is not an insurmountable barrier. Consider a memory layout where the garbage collector knew if each reference was being used in a read-only situation (i.e. `alert(constants.pi)`) or a read-write situation (i.e. `constants.pi = 3`). From this information it would be able to determine which variables were only used read-only and thus were guaranteed to be constant. If `constants` and `constants.pi` were both detected as read-only then the field lookup could be performed by the garbage collector, freeing both `constants` and `fiveDigitPrimes`. If the dictionary implementation provided certain guarantees then even if only `constants` was constant the lookup would still be safe.

In Haskell lazy evaluation is common (the default) and space leaks due to selectors are unavoidable, making the decision to apply selector optimisation obvious. In languages like Javascript adding additional code to solve fixable space leaks at the cost of making the normal code slower or more complex may not be a sensible trade-off.

### Detecting space leaks

We have seen five examples of space leaks, hopefully providing some intuition as to where space leaks occur and how they can be fixed. However, all the examples have been a handful of lines - for space leaks in big programs the challenge is often finding the code at fault. As Haskell is particularly vulnerable to space leaks the compiler provides a number of built in profiling tools to pinpoint their source. But before we look at which tools are available, let us first consider which tools might be useful.

Space leaks are quite different to memory leaks, in particular the garbage collector still knows about the memory, and will usually free the memory before the program terminates. Assuming a definition of `sum` containing a space leak, as soon as `sum` has produced a result, any intermediate space leak will be freed by the garbage collector. A program with a space leak will often reach its peak memory use in the middle of the execution, compared to memory leaks which never decrease. A standard technique for diagnosing memory leaks is to look at the memory after the program has finished, to see what is unexpectedly retained - such a technique is less useful for space leaks.

Instead, it is often useful to examine the memory at intermediate points throughout the execution, looking for spikes in the memory usage. Capturing the entire memory at frequent intervals is likely to require too much disk space, so one solution is to record summary statistics such as how much memory was allocated by each function at regular intervals.

#### Haskell Tools

The Haskell compiler provides several profiling modes to generate and graph heap summary information. To generate a profile we first compile our program with the following flags:

    ghc --make Main.hs -prof -fprof-auto -fprof-cafs -rtsopts

These flags mean:

* `ghc --make Main.hs` - compile the file `Main.hs` into an executable, as normal.
* `-prof -fprof-auto -fprof-caf` - turn on profiling in the executable, and make sure it is able to record information about top-level definitions.
* `-rtsopts` - allow the resulting executable to accept profiling options.

We can run the resulting program as normal, but with additional flags we can also generate profile information:

    main +RTS -xt -hy
    hp2ps -c main.hp

If we use the `mean` example from earlier we produce the first plot shown in Figure 1. The first command runs the resulting `main` executable with some flags to the runtime system (anything after `+RTS`). The `-xt` flag says include the stack in the profile output (this author believes `-xt` should be on by default) and `-hy` to generate a report summarised by type. The first command generates a file `main.hp` and the second command turns that into a PostScript file `main.ps` (in color, due to the `-c` flag). In the plots shown I also passed `-i0.01` to sample the memory more frequently, which is usually only necessary when trying quick-running toy examples.

Haskell has a number of profiling modes, and the best approach is to try them all and see which produces the most useful information. The four standard types of profile are shown in Figure 1. They are:

* `-hy` summarises the memory by type, in the example we have some lists (`[]`) and some numbers (`Int`). This summary answers the question "what" is in the heap.
* `-hd` summarises by description, showing a more refined version of `-hy`. In the example we can see a close correspondence to `-hy`, with all `Int` entries matching `I#` (which is the internal constructor of `Int`), and lists matching `(:)`. Any group below a threshold is hidden, or we would likely see a single `[]` denoting the end of the list.  
* `-hc` summarises by cost-centre. A cost-centre is a named area of the source code, automatically inserted on all top-level definitions, and can also be manually inserted with an annotation in the code. In the example we can see that `main` has been attributed all the allocation, likely due to optimisation inlining `mean` inside it. This summary answers the question "where" was the memory created.
* `-hm` summarises by module. A module is a more granular version of cost-centre.

From a combination of these plots we can see that the function `main` in the module `Main` allocates a large list of numbers. It allocates the list over 0.4 seconds, then quickly consumes the list over 0.1 seconds. This heap usage describes what we would expect from the original definition of `mean`.

For larger programs the plot will often contain a lot of memory usage that is expected, and not relevant to the space leak. To simplify the plot we can filter by any of the four types, for example passing `-hc -hy[]` will show a plot grouped by cost-centre, but only where the type is a list.

As we have seen in the `sum` example, compiling with different optimisation settings may cause space leaks to appear or disappear, and sadly compiling for profiling can have similar effects (although it is relatively rare). As a fallback any Haskell executable can be run using `+RTS -hT` which produces a plot summarised by type, without compiling for profiling and causing few changes to the behaviour of the program.

Using the profiling tools to their full effect requires practice and perseverance. To get a better idea of how real problems are tackled using these tools I recommend the following two "tales from the trenches": 

* <http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/>
* <http://book.realworldhaskell.org/read/profiling-and-optimization.html>

#### Javascript Tools

One tool Haskell lacks is the ability to pause execution at a certain point and explore the memory. In contrast, such a feature is available in some Javascript implementations, for example the Chrome heap profiler.    

The Chrome heap profiler allows a snapshot of the heap to be taken and explored. The profiler shows a tree of the heap, showing which values point at each other. You can summarise by the type of object, see statistics about how much memory is consumed and referenced by a certain value, and filter by name. A feature particularly useful for diagnosing space leaks is the ability to see the retaining tree of a value. The two Javascript space leaks in this article produce heaps which pinpoint the problem easily.

### Are space leaks inevitable?

Garbage collection frees programmers from the monotony of manually managing memory, making it easier for languages to include advanced features like lazy evaluation or closures. These advanced features abstract away from the memory layout, making it harder to predict what memory looks like, potentially leading to space leaks.

Compilers for lazy functional languages have been dealing with space leaks for over thirty years and have developed a number of strategies to help. There hare been changes to compilation techniques, modifications to the garbage collector and tools to detect space leaks when they do occur. Some of these strategies may be useful to other languages.

While space leaks are worrisome, they are not fatal, they can be detected and eliminated, and are the cost of certain language features and should be viewed as a trade-off - certain advanced language features lead to space leaks. Haskell has been used successfully in many projects (cite CUFP). In the same way that anyone can write an _O(n^2)_ sorting algorithm, anyone can write an _O(n)_ summation algorithm.

I have three hopes for the future:

* In particular domains people will work to eliminate space leaks by construction, for example FRP and pipes/conduits/enumerators. Generally I hope people can come up with nice libraries that are correct by construction.

* Determine you have a space leak. This step is usually performed after a user reports a program using too much space, there are no practical tools for stating what the expecting space usage is, and for annotating methods that take "suspiciously high" space.

* The tooling could be improved so that once you knew you had a space leak it could be fixed. As an example, this author spent many days tracking down a space leak. Part of the problem is that you need robust tools, not just academic theory, and as Haskell has been an academic language, the tools have lagged behind. In addition, most authors occasionally track down a space leak, it is not a regular occurrence, so there is no particular itch to scratch.
