# Maximum Patches, Minimum Time

_Summary: Writing a fast continuous integration server is tricky._

When I started writing the continuous integration server Bake, I thought I had a good idea of what would go fast. It turned out I was wrong. The problem Bake is trying to solve is:

* You have a set of tests that must pass, which take several hours to run.
* You have a current state of the world, which passes all the tests.
* You have a stream of incoming patches that can be applied to the current state, there may be hundreds of patches per day.
* You want to advance the state by applying patches, ensuring the current state always passes.
* You want to reject bad patches and apply good patches as fast as possible, which involves running as few tests as possible.

While test dependencies can describe properties such as you must compile before running any tests, it turns out compilation is quite special due to its incremental nature.

#### The (wrong) Solution

My initial solution was based on the notion of a frontier - a sequence of patches applied to the current state that is the focus of testing. Each potential state (a state plus a sequence of patches) operated in its own directory. When created, each directory is copied from the nearest available potential state so that incremental compilation can reduce rebuilding.

The strategy was that each incoming patch was immediately included in the frontier, compiled, and run on all tests. I would always run the test that had not passed for the longest time, to increase confidence in more patches. Concretely, if I have tested `T1` on patch `P1`, and `P2` comes in, I start testing `T2` on the combination of `P1+P2`. After that passes I can be somewhat confident that `P1` passes both `T1` and `T2`, despite not having run `T2` on just `P1`.

If a test fails, I bisect to find the patch in question, reject it, and immediately throw it out of the frontier.

#### The Problems

There are several problems with this approach:

* I'm regularly throwing patches out of the frontier, or bisecting over subsets of the frontier, each requires a significant amount of compilation, as it has to recompile all subsequent patches.
* I'm regularly adding patches to the frontier, each of which requires an incremental compilation. However, copying a directory of lots of small files (the typical output of a compiler) is fantastically expensive on Windows, so even when incremental compilation is fast, the initialisation time is large.

The basic problem ended up being that the strategy spent all time copying and compiling, and relatively little time testing.

#### A Better Approach

My improved approach is to take a set of patches and consider them, running all tests until they all pass, without extending to the frontier. When all the tests pass on the current frontier, I make that the new state, and extend the frontier with all new patches.

In order to benefit from incremental compilation, I always compile in the same directory, and I compile each patch in series, then zip up the interesting files (the test executables and test data). Concretely, given `P1+P2+P3`, I compile `P1` and zip the results, then `P1+P2` and zip, then `P1+P2+P3` and zip. To bisect, I just unzip the relevant directory. It turns out that on Windows, creating the zip is relatively quick, but extracting it is much more expensive, but that only needs to be done if a bisection is required, and only for _O(log n)_ points. I also only need to zip the stuff required for testing, not for building, which is often much smaller.

When testing a frontier, if nothing fails, I update the state. If anything fails I bisect to figure out who should be rejected, but don't reject until I've completed all tests. After identifying all failing tests, and the patch that caused each of them to fail, I throw those patches out of the frontier. I then rebuild with the revised frontier and run only those tests that failed last time around, trying to seek out tests where two independent patches in a frontier both broke them. I repeat with an increasingly small set of tests that failed last time, until no tests fail. Once there are no failing tests, I extend the frontier with all new patches, but do not update the state.

As a small tweak, if there are two patches in the queue from the same person, where one is a superset of the other, I ignore the subset. The idea is that if the base commit has an error I don't want to track it down twice, once to the first failing commit and then again to the second one.

#### Using this approach in Bake

First, the standard disclaimer: Bake may not meet your needs - it is a lot less developed than other continuous integration systems. If you do decide to use Bake, you should run from the git repo, as the Hackage release is far behind.

In Bake this approach is implemented in the `StepGit` module, with the `ovenStepGit` function. Since Bake doesn't have the notion of building patches in series it pretends (to the rest of Bake) that it's building the final result, but secretly caches the intermediate steps. If there is a failure when compiling, it caches that failure, and reports it to each step in the bisection, so Bake tracks down the correct root cause.

I am currently recommending `ovenStepGit` as the "best" approach for combining `git` and an incremental build system with Bake. While any incremental build system works, I can't help but plug [Shake](http://shakebuild.com), because its the best build system I've ever written.
