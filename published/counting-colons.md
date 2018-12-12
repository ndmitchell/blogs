# Counting the Cost of Colons

_Summary: Haskell uses :: as the type operator. That was a mistake that costs us over 1 million characters of source code._

Haskell uses `::` for type annotations, e.g. `(1 :: Int)`. Most other FP languages with types use `:`, including Scala, OCaml, Agda, Idris and Elm. Haskell uses `:` for list cons, so you can write:

```
myList = 1:2:[] :: [Int]
```

Whereas in other languages you write:

```
myList = 1::2::[] : [Int]
```

Moreover, the reason Haskell preferred `::` was the belief that if the cons operator was `::` then people would quite naturally insert spaces around it, giving:

```
myList = 1 :: 2 :: [] : [Int]
```

The final program is now noticeably longer than the original. Back when people first invented Haskell, I imagine they were mainly list manipulation operations, whereas now plenty of libraries work mainly at the type level. That raises the question - would [Hackage](https://hackage.haskell.org/) be shorter or longer if we used `:` for types?

**Method**

I [downloaded the latest version of every Hackage package](https://neilmitchell.blogspot.com/2018/11/downloading-all-of-hackage.html). For each `.hs` file, I excluded comments, then counted the number of instances of `:` and `::`, also noting whether there were spaces around the `:`. The code I used can be found [here](https://gist.github.com/ndmitchell/f758711a017429b514e66ff4057730f1).

**Results**

* Instances of `::` = 2,015,418
* Instances of `:` = 589,318 (of which 85,347 were surrounded by two spaces, and 193,287 had one space)

**Discussion**

Assuming we didn't add/remove any spaces, switching the symbols would have saved 1,456,100 characters. If we assume that everyone would have written `::` with spaces, that saving drops to 611,445 characters. These numbers are fairly small, representing about 0.15% of the total 993,793,866 characters on Hackage.

**Conclusion**

The Haskell creators were wrong in their assumptions - type's should have been `:`.
