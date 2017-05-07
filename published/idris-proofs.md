# Proving stuff with Idris

_Summary: I've been learning Idris to play around with writing simple proofs. It seems pretty cool._

The [Idris programming language](https://www.idris-lang.org/) is a lot like Haskell, but with quite a few cleanups (better export syntax, more though out type classes), and also dependent types. I'm not a massive fan of dependent types as commonly presented - I want to write my code so it's easy to prove, not intertwine my beautiful code with lots of invariants. To try out Idris I've been proving some lemmas about list functions. I'm very much an Idris beginner, but thought I would share what I've done so far, partly to teach, partly to remember, and party to get feedback/criticism about all the beginner mistakes I've made.

Before proving stuff, you need to write some definitions. Idris comes with an append function (named `++` as you might expect), but to keep everything simple and out in the open I've defined:

	append : List a -> List a -> List a
	append [] ys = ys
	append (x :: xs) ys = x :: append xs ys

Coming from Haskell the only real difference is that cons is `::` and types are `:`. Given how Haskell code now looks, that's probably the right way around anyway. We can load that code in the Idris interactive environment and run it:

    $ idris Main.idr
	Main> append [1,2] [3]
    [1,2,3] : List Integer

What we can also do, but can't easily do in Haskell, is start proving lemmas about it. We'd like to prove that `append xs [] = xs`, so we write a proof:

    proof_append_nil : (xs : List a) -> append xs [] = xs
    proof_append_nil xs = ?todo

We name the proof `proof_append_nil` and then say that given `xs` (of type `List a`) we can prove that `append xs [] = xs`. That's pretty direct and simple. Now we have to write the proof - we first write out the definition leaving `?todo` where we want the proof to go. Now we can load the proof in Idris and type `:t todo` to get the bit of the proof that is required, and Idris says:

    todo : append xs [] = xs

That's fair - we haven't proven anything yet. Since we know the proof will proceed by splitting apart the `xs` we can rewrite as:

    proof_append_nil [] = ?todo_nil
    proof_append_nil (x :: xs) = ?todo_cons

We can now ask for the types of the two remaining `todo` bits:

    todo_nil : [] = []
    todo_cons : x :: append xs [] = x :: xs

The first `todo_nil` is obviously true since the things on either side of the equality match, so we can replace it with `Refl`. The second statement looks like the inductive case, so we want to apply `proof_append_nil` to it. We can do that with `rewrite proof_append_nil xs`, which expands to `append xs [] = xs`, and rewrites the left-hand-side of the proof to be more like the right. Refined, we end up with:

    proof_append_nil [] = Refl
    proof_append_nil (x :: xs) = rewrite proof_append_nil xs in ?todo_cons

Reloading again and asking for the type of `todo_cons` we get:

    todo_cons : x :: xs = x :: xs

These things are equal, so we replace `todo_cons` with `Refl` to get a complete proof of:

	proof_append_nil : (xs : List a) -> append xs [] = xs
	proof_append_nil [] = Refl
	proof_append_nil (x :: xs) = rewrite proof_append_nil xs in Refl

**Totality Checking**

Proofs are only proofs if you have code that terminates. In Idris, seemingly sprinkling the statement:

    %default total

At the top of the file turns on the totality checker, which ensures the proof is really true. With the statement turned on I don't get any warnings about totality issues, so we have proved `append` really does have `[]` as a right-identity.

**Avoiding rewrite**

The `rewrite` keyword seems like a very big hammer, and in our case we know exactly where we want to apply the rewrite. Namely at the end. In this case we could have equally written:

    cong (proof_append_nil xs)

Not sure which would be generally preferred style in the Idris world, but as the proofs get more complex using `rewrite` certainly seems easier.

**Differences from Haskell**

Coming from a Haskell background, and sticking to simple things, the main differences in Idris were that modules don't have export lists (yay), lists are `::` and types are `:` (yay), functions can only use functions defined before them (sad, but I guess a restriction to make dependent types work) and case/lambda both use `=>` instead of `->` (meh).

**Next steps**

For my first task in Idris I also defined two `reverse` functions:

	reverse1 : List a -> List a
	reverse1 [] = []
	reverse1 (x :: xs) = reverse1 xs `append` [x]
	
	rev2 : List a -> List a -> List a
	rev2 acc [] = acc
	rev2 acc (x :: xs) = rev2 (x :: acc) xs
	
	reverse2 : List a -> List a
	reverse2 = rev2 []

The first `reverse1` function is `reverse` in _O(n^2)_, the second is `reverse` in `O(n)`. I then tried to prove the three lemmas:

	proof_reverse1_reverse1 : (xs : List a) -> xs = reverse1 (reverse1 xs)
    proof_reverse2_reverse2 : (xs : List a) -> xs = reverse2 (reverse2 xs)
	proof_reverse1_reverse2 : (xs : List a) -> reverse1 xs = reverse2 xs

Namely that both `reverse1` and `reverse2` are self inverses, and then that they are equivalent. To prove these functions required a few helper lemmas, but only one additional Idris function/feature, namely:

    sym : (left = right) -> right = left

A function which transforms a proof of equality from one way around to the other way around. I also required a bunch of helper lemmas, including:

    proof_append_assoc : (xs : List a) -> (ys : List a) -> (zs : List a) -> append xs (append ys zs) = append (append xs ys) zs

Developing these proofs in Idris took me about ~2 hours, so were a nice introductory exercise (with the caveat that I've proven these lemmas before, although not in 4+ years). I'd invite anyone interested in learning this aspect of Idris to have a go, and I'll post my proofs sometime in the coming week.
