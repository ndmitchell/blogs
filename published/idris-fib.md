# Proving fib equivalence

_Summary: Using Idris I proved the exponential and linear time `fib` functions are equivalent._

The Haskell wiki proclaims that [Implementing the Fibonacci sequence is considered the "Hello, world!" of Haskell programming](https://wiki.haskell.org/The_Fibonacci_sequence). It also provides multiple versions of `fib` - an exponential version and a linear version. We can translate these into [Idris](https://www.idris-lang.org/) fairly easily:

	fibExp : Nat -> Nat
	fibExp Z = 0
	fibExp (S Z) = 1
	fibExp (S (S n)) = fibExp (S n) + fibExp n
	
	fibLin' : Nat -> Nat -> Nat -> Nat
	fibLin' Z a b = b
	fibLin' (S n) a b = fibLin' n (a + b) a
	
	fibLin : Nat -> Nat
	fibLin n = fibLin' n 1 0

We've made the intermediate `go` function in `fibLin` top-level, named it `fibLib'` and untupled the accumulator - but it's still fundamentally the same. Now we've got the power of Idris, it would be nice to prove that `fibExp` and `fibLin` are equivalent. To do that, it's first useful to think about why `fibLib'` works at all. In essence we're decrementing the first argument, and making the next two arguments be `fib` of increasingly large values. If we think more carefully we can come up with the invariant:

    fibLinInvariant : (d : Nat) -> (u : Nat) ->
        fibLin' d (fibExp (1+u)) (fibExp u) = fibExp (d+u)

Namely that at all recursive calls we must have the `fib` of two successive values (`u` and `u+1`), and after `d` additional loops we end up with `fib (d+u)`. Actually proving this invariant isn't too hard:

	fibLinInvariant Z u = Refl
	fibLinInvariant (S d) u =
	    rewrite fibLinInvariant d (S u) in
	    rewrite plusSuccRightSucc d u in
	    Refl

Idris simplifies the base case away for us. For the recursive case we apply the induction hypothesis to leave us with:

    fibExp (plus d (S u)) = fibExp (S (plus d u))

Ignoring the `fibExp` we just need to prove `d+(u+1) = (d+u)+1`. That statement is obviously true because `+` is associative, but in our particular case, we use `plusSuccRightSucc` which is the associative lemma where `+1` is the special `S` constructor. After that, everything has been proven.

Armed with the fundamental correctness invariant for `fibLib`, we can prove the complete equivalence.

	fibEq : (n : Nat) -> fibLin n = fibExp n
	fibEq n =
	    rewrite fibLinInvariant n 0 in
	    rewrite plusZeroRightNeutral n in
	    Refl

We appeal to the invariant linking `fibLin'` and `finExp`, do some minor arithmetic manipulation (`x+0 = x`), and are done. Note that depending on exactly how you define the `fibLinInvariant` you require different arithmetic lemmas, e.g. if you write `d+u` or `u+d`. Idris is equipped with everything required.

I was rather impressed that proving `fib` equivalence wasn't all that complicated, and really just requires figuring out what fundamental property makes `fibLin` actually work. In many ways the proof makes things clearer.
