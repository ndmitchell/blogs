# Finding a GHC bug

_Summary: I found a nasty bug in GHC 7.10 RC3. It's been fixed._

For [Shake](http://shakebuild.com/), I have an extensive test suite (2500+ lines of tests). I also test on 7 [GHC](https://www.haskell.org/ghc/) versions, including GHC HEAD. After adding GHC 7.10 Release Candidate 3 to the mix one of the tests started failing. A week later the bug has been simplified, diagnosed and fixed as bug [#10165](https://ghc.haskell.org/trac/ghc/ticket/10165). Below is a tale of how that happened, including a technical explanation of the bug in Step 8.

**Step 1: Write a lot of tests**

The Shake test that caught this particular bug checks that if the user makes a mistake then the error message must contain certain substrings correctly identifying the problem. With GHC 7.10 RC3 on [Travis](https://travis-ci.org/ndmitchell/shake) this test stopped throwing an exception entirely, continuing as though nothing were wrong. Weird.

**Step 2: Reproduce locally**

I tried to reproduce the failure locally, which ended up spotting [a fatal bug](https://ghc.haskell.org/trac/ghc/ticket/10165) in the GHC 7.10 RC3 32bit Windows version. After opting for the 64bit version, at first I couldn't reproduce the error. Eventually I realised that you needed to turn on optimisation (at `-O1`), and that running through `ghci` (how I usually develop Haskell) didn't cause the problem. Noticing that `-O1` was required gave me a clue, that it was related to an optimisation. The typical cause of programs that work without optimisation but fail with it are programs that raise exceptions in pure code (since the exception can change due to optimisations) or those that call `unsafePerformIO` (it has `unsafe` in the name for a reason). I certainly do both those things in Shake, but I wasn't aware of anywhere I did them in a dubious manner.

**Step 3: Reduce the test case**

I spent a lot of time trying to reduce the test case. By inserting `print` statements I narrowed the place the difference was happening to [`Development.Shake.Core.applyKeyValue`](https://github.com/ndmitchell/shake/blob/master/src/Development/Shake/Core.hs#L536), which is a pretty core bit of Shake. However, while I was able to chop out a lot of auxiliary features (lint tracking, command tracing) the actual code remained difficult to reduce to any great extent, for two reasons. Firstly, the bug was incredibly fragile - moving a monomorphic `NOINLINE` function from one module to another made the bug disappear. Secondly, the `applyKeyValue` function is right in the middle of Shake, and the test required a few successful Shake runs to set up things for the failing test, so I couldn't change its observable semantics too much.

What I did conclude was that Shake didn't seem to be doing anything dodgy in the small patch of code that seemed relevant, giving me the first hint that maybe GHC was at fault, not Shake. 

**Step 4: Differences at the Core level**

At this point, I reached out to the GHC mailing list, [asking if anyone had any ideas of a culprit](http://osdir.com/ml/general/2015-03/msg23402.html). They didn't, but Simon Peyton Jones suggested finding the smallest breaking change and comparing the generated Core. You can do that by compiling with `-ddump-simpl`, and adding `-dsuppress-all -dsuppress-uniques` to get something a bit easier to diff. Fortunately, by this point I had a very small change to make the error appear/disappear (moving a function from one module to another), so the difference in Core was tiny. The change in the problematic version read:

    case (\_ -> error "here") of {}

In GHC Core a `case` always evaluates its scrutinee until it has the outermost value available (aka WHNF). The empty alternatives mean that GHC has proven that the evaluation always results in an exception. However, a lambda already has a value available (namely the lambda) so evaluation never throws an exception. As a result, GHC has violated the rules of Core and bad things happen.

**Step 5: Reducing further**

In order to reduce the bug further I now had a better test, namely:

    ghc Core.hs -O -ddump-simpl | grep -F "case (\\"

With this test I didn't have to keep the internals of Shake working, and in fact didn't even have to provide a runnable entry point - all I had to do was look for the dodgy construction in the Core language. Note that I'm not actually looking for `case` of a lambda with empty alternatives, reasoning (seemingly correctly) that any `case` on a lambda with non-empty alternatives would be eliminated by the GHC simplifier, so any `case` followed by lambda is buggy.

I reduced by having a [`ghcid`](https://github.com/ndmitchell/ghcid) Window open in one corner, using the warnings `-fwarn-unused-binds` and `-fwarn-unused-imports`. I hacked out some part of the program and then patched everything up so it no longer raised an error using `ghcid` for rapid feedback. I then ran the `grep` test. If the bug had gone I put the program back to how it was and tried somewhere else. If the bug remained I then cleaned up the now redundant declarations and imports and checked again, repeating until the code was minimal.

Several hours later I was left with something like:

	buggy :: (() -> Bool) -> () -> Bool -> IO ()
	buggy fun unit bool =
	    runReaderT (
	        (if bool then liftIO $ print () else p) >>
	        (if fun unit then error2Args unit unit >> p else p)) ()

    {-# NOINLINE error2Args #-}
    error2Args :: () -> () -> a
    error2Args _ _ = error "here"

Note that `error2Args` must be in a different module to `buggy`.

**Step 6: Bisecting**

At this point [hvr](https://github.com/hvr) stepped in and bisected all the changes between GHC 7.10 RC2 and RC3, determining that a [large `Typeable` change](http://git.haskell.org/ghc.git/commitdiff/6f46fe15af397d448438c6b93babcdd68dd78df8) introduced the bug in the original `shake` test case. However, using the minimal program, the bug was also present in GHC 7.10 RC2. That suggested the bug might have been around for a while.

**Step 7: Augmenting GHC's Lint Checker**

GHC already has a pass in the compiler, enabled with `-dcore-lint`, which checks for dodgy constructs in the Core language. Enabling it didn't pick up this example (hence I used `grep` instead), so [Joachim Breitner](https://github.com/nomeata) added such a check. He also added the example as a test case, so that if it ever breaks in future things it will be spotted immediately.

**Step 8: Diagnose and Fix**

Joachim then continued to diagnose and fix the issue, the details of which can be found [in the patch](https://phabricator.haskell.org/D747). The problem (as I understand it) is that GHC looks at the code:

    fun x = error "foo" x

And concludes two facts.

1. If `fun` is called with one argument then the code will raise an error. That's true, and allows the compiler to replace `fun () ()` with `fun ()`.
2. After analysing all calls of `fun` it spots that `fun` is always called with two arguments, so it is free to change `fun` to be `fun x y = error "foo" x y`.

By applying these two facts, we can make the transformation:

    case fun () () of {}
    -- apply the first rule
    case fun () of {}
    -- inline fun after applying the second rule
    case (\x y -> error "foo" x y) () of {}
    -- beta reduce:
    case (\y -> error "foo" () y) of {}

Now we have caused invalid Core to be produced. While the two facts are each individually correct, applying the first fact causes the second fact to stop being true. Joachim fixed this by making the call argument count analysis stop at the first argument that guarantees an error.

**Step 9: The Impact**

The manifestation of the bug is quite interesting. Essentially GHC decides something is an error, but then fails to actually throw the error. As a result, any code the simplifier places after the error call will be eliminated, and that can remove a large chunk of the program. However, any code the simplifier doesn't manage to hoist next to the code will still get run, even though it should have been skipped due to an error. In essence, given exactly the wrong conditions to trigger the bug, you can write:

    main = do
        putStrLn "here1"
        ... error "foo" ...
        putStrLn "here2"
        ...
        putStrLn "here3"

And end up with the program printing `here1` followed by `here3`, without throwing an exception. In the case of my original Shake test it started to compile, should have stopped with an error but instead just skipped compiling altogether and went on to do the bits after compiling. A very weird manifestation.

_Disclaimer_: I've eliminating many missteps of mine, which included pushing random patches to try and reduce on the Travis machine and installing a Linux VM.
