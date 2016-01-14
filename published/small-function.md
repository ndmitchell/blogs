# A simple Haskell function 

_Summary: An example of a small function I recently wrote - from type signature to tests._

When writing [a build system](https://github.com/snowleopard/shaking-up-ghc) there are lots of nasty corner cases to consider. One is that command line limits combined with lots of arguments sometimes requires splitting a single command up into multiple commands, each of which is under some maximum length. In this post I'll describe a function that was required, my implementation, and how I tested it.

#### Type signature and documentation

Before I even got to the function, it already had a type signature and some [Haddock](https://www.haskell.org/haddock/) documentation: 

    -- | @chunksOfSize size strings@ splits a given list of strings into chunks not
    --   exceeding @size@ characters. If that is impossible, it uses singleton chunks.
    chunksOfSize :: Int -> [String] -> [[String]]

As an example:

    chunksOfSize 5 ["this","is","a","test"] == [["this"],["is","a"],["test"]]

#### Implementation

My implementation was:

    chunksOfSize n = repeatedly $ \xs ->
        let i = length $ takeWhile (<= n) $ scanl1 (+) $ map length xs
        in splitAt (max 1 i) xs

First we use the [`repeatedly` function](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:repeatedly) from the [`extra` library](https://github.com/ndmitchell/extra). This has the signature:

    repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]

Given a list of input, you supply a function that splits off an initial piece and returns the rest. One of the examples in the documentation is:

    repeatedly (splitAt 3) xs  == chunksOf 3 xs

So we can see how `repeatedly` lets us focus on just the "next step" of this list, ignoring the recursion. For the function argument we have two tasks - first decide how many items to put in this chunk, then to split the chunks. Splitting the chunks is the easy bit, and can be written:

    splitAt (max 1 i) xs

If we know the next `i` elements will be at or below the limit, then we can use `splitAt` to divide the elements. As a special case, if no elements would be allowed, we allow one, using `max 1` to ensure we never pass `0` to `splitAt` (and thus enter an infinite loop). That leaves us with:

    i = length $ takeWhile (<= n) $ scanl1 (+) $ map length xs

Reading from right to left, we reduce each element to it's `length`, then use `scanl1` to produce a running total - so each element represents the total length up to that point. We then use `takeWhile (<= n)` to keep grabbing elements while they are short enough, and finally length to convert back to something we can use with `splitAt`.

#### Tests

When testing, I tend to start with a few concrete examples then move on to [QuickCheck](https://hackage.haskell.org/package/QuickCheck) properties. As an initial example we can do:

    quickCheck $
        chunksOfSize 3 ["a","b","c","defg","hi","jk"] ==
        [["a","b","c"],["defg"],["hi"],["jk"]]

Here we are explicitly testing some of the corner cases - we want to make sure the full complement of 3 get into the first chunk (and we haven't got an off-by-one), we also test a singleton chunk of size 4. Now we move on to QuickCheck properties:

    quickCheck $ \n xs ->
        let res = chunksOfSize n xs
        in concat res == xs &&
           all (\r -> length r == 1 || length (concat r) <= n) res

There are really two properties here - first, the chunks `concat` together to form the original. Secondly, each chunk is either under the limit or a singleton. These properties capture the requirements in the documentation.

A final property we can check is that it should never be possible to move the first piece from a chunk to the previous chunk. We can write such a property as: 

    all (> n) $ zipWith (+)
        (map (sum . map length) res)
        (drop 1 $ map (length . head) res)

This property isn't as important as the other invariants, and is somewhat tested in the example, so I didn't include it in the test suite.

#### Performance and alternatives

The complexity is _O(n)_ in the number of `Char` values, which is as expected, since we have to count them all. Some observations about this point in the design space:

* In a strict language this would be an _O(n^2)_ implementation, since we would repeatedly `length` and `scanl` the remainder of the tail each time. As it is, we are calling `length` on the first element of each chunk twice, so there is minor constant overhead.

* Usually in Haskell, instead of counting the number of elements and then doing `splitAt` we would prefer to use `span` - something like `span ((<= n) . fst) ...`. While possible, it makes the special singleton case more difficult, and requires lots of tuples/contortions to associate each element with its rolling sum.

* For a build system, the entire input will be evaluated before, and the entire output will be kept in memory afterwards. However, if we think about this program with lazy streaming inputs and outputs, it will buffer each element of the output list separately. As a result memory would be bounded by the maximum of the longest string and the `Int` argument to `chunksOfSize`.

* It is possible to write a streaming version of this function, which returns each `String` as soon as it is consumed, with memory bounded by the longest string alone. Moreover, if the solution above was to use lazy naturals, it would actually come quite close to being streaming (albeit gaining a quadratic complexity term from the `takeWhile (<= n)`).

* The type signature could be generalised to `[a]` instead of `String`, but I would suspect in this context it's more likely for `String` to be replaced by `Text` or `ByteString`, rather than to be used on `[Bool]`. As a result, sticking to `String` seems best.   

#### Refactoring the previous version

The function [already existed](https://github.com/snowleopard/shaking-up-ghc/commit/797df55a99ffbe2fe94bae5dc202444b294ae2d0) in the codebase I was working on, so below is the original implementation. This implementation does not handle the long singleton special case (it loops forever). We can refactor it to support the singleton case, which we do in several steps. The original version was:

    chunksOfSize _    [] = []
    chunksOfSize size strings = reverse chunk : chunksOfSize size rest
      where
        (chunk, rest) = go [] 0 strings
        go res _         []     = (res, [])
        go res chunkSize (s:ss) =
            if newSize > size then (res, s:ss) else go (s:res) newSize ss
          where
            newSize = chunkSize + length s

Refactoring to use `repeatedly` we get:

	chunksOfSize size = repeatedly $ second reverse . go [] 0
	  where
	    go res _         []     = (res, [])
	    go res chunkSize (s:ss) =
	        if newSize > size then (res, s:ss) else go (s:res) newSize ss
	      where
	        newSize = chunkSize + length s

Changing `go` to avoid the accumulator we get:

	chunksOfSize size = repeatedly $ go 0
	  where
	    go _         []     = ([], [])
	    go chunkSize (s:ss) =
	        if newSize > size then ([], s:ss) else first (s:) $ go newSize ss
	      where
	        newSize = chunkSize + length s

It is then reasonably easy to fix the singleton bug:

	chunksOfSize size = repeatedly $ \(x:xs) -> first (x:) $ go (length x) xs
	  where
	    go _         []     = ([], [])
	    go chunkSize (s:ss) =
	        if newSize > size then ([], s:ss) else first (s:) $ go newSize ss
	      where
	        newSize = chunkSize + length s

Finally, it is slightly simpler to keep track of the number of characters still allowed, rather than the number of characters already produced:

	chunksOfSize size = repeatedly $ \(x:xs) -> first (x:) $ go (size - length x) xs
	  where
	    go n (x:xs) | let n2 = n - length x, n2 >= 0 = first (x:) $ go n2 xs
	    go n xs = ([], xs)

Now we have an alternative version that is maximally streaming, only applies `length` to each element once, and would work nicely in a strict language. I find the version at the top of this post more readable, but this version is a reasonable alternative.

_Acknowledgements: Thanks to [Andrey Mokhov](https://github.com/snowleopard) for providing the repo, figuring out all the weird corner cases with `ar`, and distilling it down into a Haskell problem._
