# Testing = QuickCheck + examples

Let's imagine we are testing the `sort` function. Many people will correctly say:

* It must be a permutation.
* The result must be ordered.

For `sort` that nails down all the details. But often you can't nail down all the details, short of reimplementing the function, so let's imagine something a bit weaker:

* It must be idempotent.
* It must be length preserving.

Both of these are shorter and simpler to express. Unfortunately, they both permit `id` as the function. To gain confidence we can also give a few examples:

* sort "4213" = "1234"
* sort "abbabb" = "aabbbb"

Now, given a `sort` that is not trying to be deliberately malicious, and followed an inductive pattern, it's likely that if the two examples above work, and the properties are preserved, there is a pretty good chance it is a valid sort.

Even when the properties are exhaustive (or more normally, when you think they are exhaustive but haven't actually tested that!) it can be much easier with a few examples.
