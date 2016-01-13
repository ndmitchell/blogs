# A simple Haskell function 

_Summary: An example of a small function I recently wrote - from type signature to tests._

When writing [a build system](https://github.com/snowleopard/shaking-up-ghc) there are lots of nasty corner cases to consider. One is that command line limits combined with lots of arguments sometimes requires splitting a single command up into multiple commands, each of which is under some maximum length. In this post I'll describe a function that was required, my implementation, and how I tested it.

#### Type signature and documentation

Before I even got to the function, it already had a type signature and some [Haddock](https://www.haskell.org/haddock/) documentation: 

    -- | @chunksOfSize size strings@ splits a given list of strings into chunks not
    --   exceeding the given @size@. If that is impossible, it uses singleton chunks.
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

Given a list of input, you supply a function that splits of an initial piece and returns the rest. One of the examples in the documentation is:

    repeatedly (splitAt 3) xs  == chunksOf 3 xs

So we can see how repeatedly lets us focus on just the "next step" of this list, leaving the recursion to `repeatedly`. For the function argument we have two tasks - first decide how many items to put in this chunk, then to split the chunks. Splitting the chunks is the easy bit, and can be written:

    splitAt (max 1 i) xs

If we know the next `i` elements will be at or below the limit, then we can use `splitAt` to divide the elements. As a special case, if no elements would be allowed, we allow one, using `max 1` to ensure we never pass `0` to `splitAt` (and thus enter an infinite loop). That leaves us with:

    length $ takeWhile (<= n) $ scanl1 (+) $ map length xs

Reading from right to left, we reduce each element to it's `length`, then use `scanl` to produce a running total - so each element represents the total length up to that point. We then use `takeWhile (<= n)` to keep grabbing elements while they are short enough, and finally length to convert back to something we can use.

#### Tests

When testing, I tend to start with a few concrete examples then move on to [QuickCheck](https://hackage.haskell.org/package/QuickCheck) properties. As an initial example we can do:

    quickCheck $
        chunksOfSize 3 ["a","b","c","defg","hi","jk"] ==
        [["a","b","c"],["defg"],["hi"],["jk"]]

Here we are explicitly testing some of the corner cases - we want to make sure the full complement of 3 get into the first chunk (and we haven't got an off-by-one), we test a singleton chunk of size 4. Now we move on to QuickCheck properties:

    quickCheck $ \n xs ->
        let res = chunksOfSize n xs
        in concat res == xs &&
           all (\r -> length r == 1 || length (concat r) <= n) res

There are really two properties here - first, the chunks `concat` together form the original. Secondly, each chunk is either under the limit or a singleton. These properties roughly capture the documentation invariants.

One property we don't chunk is that it should never be possible to move the first piece from a chunk to the previous chunk. That's not as important as the other invariants, and is somewhat tested in the example, so we ignore it.

#### Performance and alternatives

For a build system, the entire input will be evaluated before, and the entire output will be kept in memory afterwards. The complexity is _O(n)_ in the number of `Char` values, which is as expected, since we have to count them all. Some observations about this point in the design space:

* In a strict language this would be an _O(n^2)_ implementation
* 


length is to convert the list coming in back to an Int for the splitAt - if we had lazy Int's that would be identical to the solution before, but since we have strict Int that forces each chunk. scanl1 just gives us the running total, and takeWhile keeps taking from that total while we are below the size. In a strict language the repeated map length xs would give us O(n^2), but in Haskell we're fine.

Should be O(n) time just like the old one, and also O(n) space. If the inputs were producing streaming (which they aren't) and consumed streaming (which they aren't) then this one would force a whole chunk to be in memory at once, but otherwise they are equivalent.





#### Previous version

    chunksOfSize _    [] = []
    chunksOfSize size strings = reverse chunk : chunksOfSize size rest
      where
        (chunk, rest) = go [] 0 strings
        go res _         []     = (res, [])
        go res chunkSize (s:ss) =
            if newSize > size then (res, s:ss) else go (s:res) newSize ss
          where
            newSize = chunkSize + length s
