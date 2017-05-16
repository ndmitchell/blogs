# Idris reverse proofs

_Summary: I proved O(n) reverse is equivalent to O(n^2) reverse._

Following on from [my previous post](http://neilmitchell.blogspot.co.uk/2017/05/proving-stuff-with-idris.html) I left the goal of given two `reverse` implementations:

	reverse1 : List a -> List a
	reverse1 [] = []
	reverse1 (x :: xs) = reverse1 xs ++ [x]
	
	rev2 : List a -> List a -> List a
	rev2 acc [] = acc
	rev2 acc (x :: xs) = rev2 (x :: acc) xs
	
	reverse2 : List a -> List a
	reverse2 = rev2 []

Proving the following laws hold:

	proof_reverse1_reverse1 : (xs : List a) -> xs = reverse1 (reverse1 xs)
	proof_reverse2_reverse2 : (xs : List a) -> xs = reverse2 (reverse2 xs)
	proof_reverse1_reverse2 : (xs : List a) -> reverse1 xs = reverse2 xs

The complete proofs are available [in my Idris github repo](https://github.com/ndmitchell/idris-playground/blob/master/Reverse.idr), and if you want to get the most out of the code, it's probably best to step through the types in Idris. In this post I'll talk through a few of the details.

Directly proving the `reverse1 (reverse1 x) = x` by induction is hard, since the function doesn't really follow so directly. Instead we need to define a helper lemma.

	proof_reverse1_append : (xs : List a) -> (ys : List a) ->
        reverse1 (xs ++ ys) = reverse1 ys ++ reverse1 xs

Coming up with these helper lemmas is the magic of writing proofs - and is part intuition, part thinking about what might be useful and part thinking about what invariants are obeyed. With that lemma, you can prove by induction on the first argument (which is the obvious one to chose since `++` evaluates the first argument first). Proving the base case is trivial:

    proof_reverse1_append [] ys =
        rewrite appendNilRightNeutral (reverse1 ys) in
        Refl

The proof immediately reduces to `reverse1 ys == reverse1 ys ++ []` and we can invoke the standard proof that if `++` has `[]` on the right we can ignore it. After applying that rewrite, we're straight back to `Refl`.

The `::` is not really any harder, we immediately get to `reverse1 ys ++ (reverse1 xs ++ [x])`, but bracketed the wrong way round, so have to apply `appendAssociative` to rebracket so it fits the inductive lemma. The proof looks like:

	proof_reverse1_append (x :: xs) ys =
	    rewrite appendAssociative (reverse1 ys) (reverse1 xs) [x] in
	    rewrite proof_reverse1_append xs ys in Refl

Once we have the proof of how to move `reverse1` over `++` we use it inductively to prove the original lemma:

	proof_reverse1_reverse1 : (xs : List a) -> xs = reverse1 (reverse1 xs)
	proof_reverse1_reverse1 [] = Refl
	proof_reverse1_reverse1 (x :: xs) =
	    rewrite proof_reverse1_append (reverse1 xs) [x] in
	    rewrite proof_reverse1_reverse1 xs in
	    Refl

For the remaining two proofs, I first decided to prove `reverse1 x = reverse2 x` and then prove the `reverse2 (reverse2 x) = x` in terms of that. To prove equivalence of the two functions I first needed information about the fundamental property that `rev2` obeys:
	
	proof_rev : (xs : List a) -> (ys : List a) ->
        rev2 xs ys = reverse2 ys ++ xs

Namely that the accumulator can be tacked on the end. The proof itself isn't very complicated, although it does require two calls to the inductive hypothesis (you basically expand out `rev2` on both sides and show they are equivalent):

	proof_rev xs [] = Refl
	proof_rev xs (y :: ys) =
	    rewrite proof_rev [y] ys in
	    rewrite sym $ appendAssociative (rev2 [] ys) [y] xs in
	    rewrite proof_rev (y :: xs) ys in
	    Refl

The only slight complexity is that I needed to apply `sym` to switch the way the `appendAssocative` proof is applied. With that proof available, proving equivalence isn't that hard:

	proof_reverse1_reverse2 : (xs : List a) -> reverse1 xs = reverse2 xs
	proof_reverse1_reverse2 [] = Refl
	proof_reverse1_reverse2 (x :: xs) =
	    rewrite proof_rev [x] xs in
	    rewrite proof_reverse1_reverse2 xs in
	    Refl

In essence the `proof_rev` term shows that `rev` behaves like the _O(n^2)_ reverse.

Finally, actually proving that `reverse2 (reverse2 x)` is true just involves replacing all the occurrences of `reverse2` with `reverse1`, then applying the proof that the property holds for `reverse1`. Nothing complicated at all:
	
	proof_reverse2_reverse2 : (xs : List a) -> xs = reverse2 (reverse2 xs)
	proof_reverse2_reverse2 xs =
	    rewrite sym $ proof_reverse1_reverse2 xs in
	    rewrite sym $ proof_reverse1_reverse2 (reverse1 xs) in
	    rewrite proof_reverse1_reverse1 xs in
	    Refl

If you've got this far and are still hungry for more proof exercises I recommend [Exercises on Generalizing the Induction Hypothesis](https://homes.cs.washington.edu/~jrw12/InductionExercises.html) which I have now worked through ([solutions](https://github.com/ndmitchell/idris-playground/blob/master/Generalising.idr) if you want to cheat).
