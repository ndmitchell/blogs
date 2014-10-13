# Shake's Internal State

_Summary: Shake is not like Make, it has different internal state, which leads to different observable behaviour. I also store the state in an optimised way._

In order to understand the behaviour of Shake, it is useful to have a mental model of Shake's internal state. To be a little more concrete, let's talk about `File`s which are stored on disk, which have `ModTime` value's associated with them, where `modtime` gives the `ModTime` of a `FilePath` (Shake is actually generalised over all those things). Let's also imagine we have the rule:

    file *> \out -> do
        need [dependency]
        run

So `file` depends on `dependency` and rebuilds by executing the action `run`.

**The Make Model**

In Make there is no additional state, only the file-system. A file is considered dirty if it has a dependency such that:

    modtime dependency > modtime file

As a consequence, `run` _must_ update `modtime file`, or the file will remain dirty and rebuild in future runs.

**The Shake Model**

For Shake, the state is:

    database :: File -> (ModTime, [(File, ModTime)])

Each `File` is associated with a pair containing the `ModTime` of `file` when the rule was last built, plus a list of each dependency and the `modtime` when it built. As part of executing the rule above, Shake records the association:

    file -> (modtime file, [(dependency, modtime dependency)])

The `file` is considered dirty if any of the information is no longer current. In this example, if `modtime file` changes or `modtime dependency` changes.

There are a few consequences of the Shake model:

* There is no requirement for `modtime file` to change as a result of `run`. The file is dirty because something changed, after we run the rule and record new information it becomes clean.
* Since a file may not change its `modtime`, things that depend on `file` may not require rebuilding even if `file` rebuilds.
* If you update an output file, it will rebuild that file, as the `ModTime` of a result is tracked.
* Shake only ever performs equality tests on `ModTime`, never ordering, which means it generalises to other types of value and works even if your file-system sometimes ends up with incorrect times.

These consequences allow two workflows that aren't really feasible in Make:

* Generated files, where the generator changes often, but the output of the generator for a given input changes rarely. In Shake, you can rerun the generator regularly, and using a function that writes only on change (`writeFileChanged` in Shake) you don't rebuild further. This technique can reduce some rebuilds from hours to seconds.
* Configuration file splitting, where you have a configuration file with lots of key/value pairs, and want certain rules to only depend on a subset of the keys. In Shake, you can generate a file for each key/value and depend only on that key. If the configuration file updates, but only a subset of keys change, then only a subset of rules will rebuild. Alternatively, using `Development.Shake.Config` you can avoid the file for each key, but the dependency model is the same.

**Optimising the Model**

The above model expresses the semantics of Shake, but the implementation uses an optimised model. Note that the original [Shake paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) gives the optimised model, not the clean model - that's because I only figured out the difference a few days ago (thanks to Simon Marlow, Simon Peyton Jones and Andrey Mokhov). To recap, we started with:

    database :: File -> (ModTime, [(File, ModTime)])

We said that `File` is dirty if any of the `ModTime` values change. That's true, but what we are really doing is comparing the `fst` `ModTime` with the `ModTime` _on disk_, and the list of second `ModTime`'s with those _in `database`_. Assuming we are passed the current `ModTime` on disk, then a file is valid if:

    valid :: File -> ModTime -> Bool
    valid file mNow =
        mNow == mOld &&
        and [fst (database d) == m | (d,m) <- deps]
        where (mOld, deps) = database file

The problem with this model is that we store each `File`/`ModTime` pair once for the file itself, plus once for every dependency. That's a fairly large amount of information, and in Shake both `File` and `ModTime` can be arbitrarily large for user rules.

Let's introduce two assumptions, and see what changes they suggest:

_Assumption 1:_ A `File` only has at most one `ModTime` per run Shake run, since a file will only rebuild at most once per run. We use `Step` for the number of times Shake has run on this project.

_Consequence 1:_ The `ModTime` for a file and the `ModTime` for its dependencies are all recorded in the same run, so they share the same `Step`.

_Assumption 2:_ We assume that if the `ModTime` of a `File` changes, and then changes back to a previous value, we can still treat that as dirty. In the specific case of `ModTime` that would take time travel, but even for other values (e.g. file hashes) it would require partial rebuilds.

_Consequence 2:_ We only use historical `ModTime` values to compare them for equality with current `ModTime` values. We can instead record the `Step` at which the `ModTime` last changed, assuming all older `Step` values are unequal.

The result is:

    database :: File -> (ModTime, Step, Step, [File])

    valid :: File -> ModTime -> Bool
    valid file mNow =
        mNow == mOld &&
        and [sBuild >= changed (database d) | d <- deps]
        where (mOld, sBuilt, sChanged, deps) = database file
              changed (_, _, sChanged, _) = sChanged

For each `File` we store its most recently recorded `ModTime`, the `Step` at which it was built, the `Step` when the `ModTime` last changed, and the list of dependencies. We now check if the `Step` for this rule is greater than the `Step` at which `dependency` last changed. Using the assumptions above, the original formulation is equivalent.

Note that instead of storing one `ModTime` per dependency+1, we now store exactly one `ModTime` plus two small `Step` values.

We still store each file many times, but we reduce that by creating a bijection between `File` (arbitrarily large) and `Id` (small index) and only storing `Id`.

**Implementing the Model**

For those who like concrete details, which might change at any point in the future, the relevant definition is in [Development.Shake.Database](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Database.hs#L107):

    data Result = Result
        {result    :: Value   -- the result when last built
        ,built     :: Step    -- when it was actually run
        ,changed   :: Step    -- the step for deciding if it's valid
        ,depends   :: [[Id]]  -- dependencies
        ,execution :: Float   -- duration of last run
        ,traces    :: [Trace] -- a trace of the expensive operations
        } deriving Show

The diff from the model above is:

* `ModTime` became `Value`, because Shake deals with lots of types of rules.
* The dependencies are stored as a list of lists, so we still have access to the parallelism provided by `need`, and if we start rebuilding some dependencies we can do so in parallel.
* We store `execution` and `traces` so we can produce profiling reports.
* I haven't shown the `File`/`Id` mapping here - that lives elsewhere.
* I removed all strictness/`UNPACK` annotations from the definition above.

As we can see, the code follows the optimised model quite closely.
