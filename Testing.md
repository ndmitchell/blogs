# Exceptional Testing

_Summary: Failing properties should throw exceptions, not return False._

When testing properties in Haskell with QuickCheck we usually write a predicate that takes some arguments, and returns a boolean. For example:

    import Test.QuickCheck
    main = quickCheck $ \x -> exp (log (abs x)) == abs x

Here we are checking that for all positive `Double` values, applying `log` then `exp` is the identity. This statement is incorrect for `Double` due to floating point errors. Running `main` we get:

    *** Failed! Falsifiable (after 6 tests and 2 shrinks):
    3.0

QuickCheck is an incredibly powerful tool - it first found a failing instance, and then automatically simplified our test case. However, it's left out some of the most important information - what is the value of `exp (log 3)`? For my tests I usually define `===` and use it instead of `==`:

    a === b | a == b = True
			| otherwise = error $ show a ++ " /= " ++ show (b :: Double)

Now when running `main` we get:

    *** Failed! Exception: '3.0000000000000004 /= 3.0'
    (after 2 tests and 2 shrinks):
    3.0

We can immediately see the magnitude of the error introduced, giving a kick-start to debugging.

**Do we still need `Bool`?**

When writing functions returning `Bool` there are three interesting cases:

* The function returns `True`, the test passes, continue on.
* The function returns `False`, the test fails, with no information beyond the input arguments.
* The function throws an exception, the test fails, but with a human readable error message about the point of failure.

Of these, returning `False` is the least useful, and entirely subsumed by exceptions. It is likely there was additional information available before reducing to `False`, which has now been lost, and must first be recovered before debugging can start.

Given that the only interesting values are pass and fail, it is possible to switch to using the `()` type, where passing tests return `()` and failing tests throw an exception. However, working with exceptions in pure code is a bit ugly, so I typically define:

	import Control.Monad
    (===) :: (Show a, Eq a) => a -> a -> IO ()
    a === b = when (a /= b) $ error $ show a ++ " /= " ++ show b

Now `===` is an action, and passing tests do nothing, while failing tests raise an error. This definition forces all tests to end up in `IO`, which is terrible for "real" Haskell code, where pure and `IO` computations should be segregated. However, for tests, as long as the test is repeatable (doesn't store some temporary state) then I don't worry.

To test the `IO ()` returning property with QuickCheck we can define:

    instance Testable () where
        property () = property True
    instance Testable a => Testable (IO a) where
        property = property . unsafePerformIO

Now QuickCheck can work with my `===` definition, but also any `IO` property. I have found that testing `IO` properties with QuickCheck is very valuable, even without using `===`.

**Non-QuickCheck tests**

Whilst I have argued for exceptions in the context of QuickCheck tests, I use the same exception pattern for non-parameterised/non-QuickCheck assertions. As an example, taken from [Shake]():

    test = do
        dropDirectory1 "aaa/bbb" === "bbb"
        dropDirectory1 "aaa/" === ""
        dropDirectory1 "aaa" === ""
        dropDirectory1 "" === ""

Using `IO ()` properties allows for trivial sequencing. As a result, the tests are concise, and the effort to add additional tests is low.

**(===) in QuickCheck**

In QuickCheck-2.7 the `===` operator was introduced (which caused name clashes in both [Shake]() and [Hoogle](http://haskell.org/hoogle) - new versions of both have been released). The `===` operator uses the `Property` data type in QuickCheck to pass both the `Bool` value and additional information together. I prefer my definition of `===` because it's simpler, doesn't rely on QuickCheck and makes it clear how to pass additional information beyond just the arguments to `===`. As an example, we could also return the `log` value:

    \x -> when (exp (log (abs x)) /= abs x) $
              error $ show (x,log $ abs x, exp $ log $ abs x)

However, the QuickCheck `===` is a great improvement over `==`, and should be a very popular addition.
