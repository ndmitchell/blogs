# Shake's Internal State

_Summary: Shake is not like Make, it has different internal state, which is stored in an optimised way._

In order to understand why Shake behaves the way it does, it's useful to have a mental model of Shake's internal state. For the rest of this discussion I'm going to talk about `File`'s which are stored on disk, which have `ModTime` value's associated with them, and where `modtime` gives the `ModTime` of a `FilePath`. In reality Shake is polymorphic over all those things.

Let's also imagine we have the rule:

    file: dependency
        run

So `file` depends on `dependency` and rebuilds by executing the action `run`.

**The Make Model**

In Make there is no internal state. A file is considered dirty if it has a dependency such that:

    modtime dependency > modtime file

As a consequence, `run` _must_ update `modtime file`, or the file will remain dirty and rebuild again in future runs.

**The Shake Model**

For Shake, each `File` is associated with some information:

    database :: File -> (ModTime, [(File, ModTime)])

Specifically a pair containing the `ModTime` of `file` when the rule was last built, plus a list of each dependency and the `modtime` when it built. So as part of executing `run`, Shake records:

    file -> (modtime file, [(dependency, modtime dependency)])

That is, when we build `file`, we record the `modtime` of `file`, a list of dependencies, and the `modtime` for each of the dependency. Now `file` is considered dirty if any of the information is no longer current. In this case, if `modtime file` changes or `modtime dependency` changes.

There are a few consequences of the Shake model:

* There is no requirement for a `file` to change its `modtime` in `run`. The file is dirty because something changed, we record new information, it is now clean.
* Since a file may not change its `modtime`, things that depend on `file` may not require rebuilding even if `file` rebuilds.

These consequences lead to two workflows that aren't really feasible in Make:

* Generated files, where the generator changes often, but the output of the generator for a given input changes rarely. In Shake, you can rerun the generator regularly, and using a function that only overwrites on change (`writeFileChanged` in Shake) you don't rebuild further.
* Configuration file splitting, where you have a configuration file with lots of key/value pairs, and want certain rules to only depend on a subset of the keys. In Shake, you can generate a file for each key/value and depend only on that key. If the configuration file updates, but only a subset of keys change, then only a subset of rules will rebuild. Alternatively, using `Development.Shake.Config` you can avoid the file for each key, but the idea is the same.

**Optimising the Model**

The model above for Shake entirely expresses the semantics of Shake, but the implementation has been optimised. Note that the original [Shake paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) gives the optimised model, not the clean model - that's because I only figured out the difference yesterday (thanks to Simon Marlow, Simon Peyton Jones and Andrey Mokhov). To recap, we started with:

    database :: File -> (ModTime, [(File, ModTime)])

We originally said that `File` is dirty if either of the `ModTime` values change. That's true, but what we are really doing is comparing the `fst` `ModTime` with the `ModTime` _on disk_, and the list of second `ModTime`'s with those _in `database`_. Assuming we are passed the current `ModTime` on disk, then a file is valid if:

    valid :: File -> ModTime -> Bool
    valid file mNow =
        mNow == mOld &&
        [fst (database d) == m | (d,m) <- deps]
        where (mOld, deps) = database file

Now let's make two assumptions, and see what changes they suggest:

/Assumptions 1:/ A `File` only has at most one `ModTime` per run Shake run, since once a file is made clean, we never run the rule again. We call the number of Shake runs a `Step`.

/Consequence 1:/ The `ModTime` for a file and the `ModTime` for its dependencies were all stored in the same run, so they share the same `Step`.

/Restriction 2:/ We can assume that `ModTime` if the `ModTime` changes and then changes back to a previous value we can still treat that as dirty. In the specific case of `ModTime` that would take time travel, but even for other values (e.g. file hashes) it doesn't matter.

/Consequence 2:/ We only use historical `ModTime` values to compare them for equality with current `ModTime` values. We can instead record the `Step` at which the `ModTime` last changed.

The result is:

    database :: File -> (ModTime, Step, Step, [File])

    valid :: File -> ModTime -> Bool
    valid file mNow =
        mNow == mOld &&
        [sBuild >= changed (database d) | d <- deps]
        where (mOld, sBuilt, sChanged, deps) = database file
              changed (_, _, sChanged, _) = sChanged

We now check if the `Step` for this rule is greater than the `Step` at which `dependency` last changed. Using the assumptions above, these two formulations are equivalent.

Note that instead of storing one `ModTime` per dependency+1, we now store exactly one `ModTime` plus two small `Step` values.


 Rather than storing historical `ModTime` values, so they can be compared for equality with the current 


So basically, we check the passed in `Modtime` matches what we recorded, and we check the dependencies all match `database`.

Next step is to index by revision observing there is only one revision of a `ModTime` per run.

    mtime :: File -> Revision -> ModTime

    database :: File -> (Revision, [(File, Revision)])

Next we note that actually all the `Revision`'s in a set are the same:

    mtime :: File -> Revision -> ModTime

    database :: File -> (Revision, [File])

By this stage `valid` has become:

    valid :: File -> ModTime -> Bool
    valid file mNow =
        mNow == mtime file r &&
        [mtime f (fst (database f)) == m | (f,m) <- deps]
        where (r, deps) = database file


Next we observe that there are actually only two operations we need on mtime:

    mtimeCurrent :: File -> ModTime

    mtimeEqualCurrent :: File -> Revision -> Bool

Now we assume that in revisions never change then go back to being equal:

    database :: File -> (ModTime, Revision, Revision, [File])


Where `valid` checks that a `File` and the current on-disk `ModTime` and checks that the file is consistent. This is only suitable if all the dependencies are made `valid` first.

The problem with the existing model is that we store each `File`/`ModTime` pair once for the file itself, and once for every dependency. That's a fairly large amount of information, and in Shake both values can be arbitrarily large for user rules. We can avoid the duplicated `File` values by instead assigning a small identifier to each one, which we'll call `FileId`, and storing an isomorphism between `File` and `FileId`.

Now each `File` is stored exactly once. We could use the same trick for `ModTime`, but we can actually do slightly better, avoiding an extra mapping, and adding some useful information for profiling at the same time. To proceed we need to think about what dependency operations we perform:

    valid :: FileId -> ModTime -> Bool
    valid id mNow =
        where (_, mOld, deps) = database id

    currentModtime :: FileId -> ModTime
    currentModtime f =
        where (_, m, _) = 


to make two observations:

Since a `File` will only be rebuilt at most once per Shake run, if we have a global counter of how many runs of Shake we have made (let's call it `Revision`), we can instead keep a mapping:

    values :: (FileId, Revision) -> ModTime

    database :: FileId -> (File, Revision, [(FileId, Revision)])

This step is worse, but sets us up for the next step. We're really interested in asking two questions:

* Give a `FileId`, what is the current `ModTime`. To find that, we get the `Revision` for the `FileId`, then look up the `ModTime`.
* Given a `(FileId, Revision)` pair, is that still valid. To find that, we get the current `ModTime` for the `FileId` (as above), then just look up the pair we have directly, and compute whether the two are equal.

    currentValue :: FileId -> ModTime
    currentValue id = map2 (id, r)
        where (_, r, _) = map1 id

    consistentValue :: FileId -> Revision -> Bool
    consistentValue id r = currentValue id == map2 (id, r)

So now, provided we can still provide those two functions, we are safe to optimise the implementation.

The key observation is that if a value changes, and then changes back, we don't really care about what `consistentValue` returns. For `ModTime` changing back would imply time travel. Therefore, we can instead store the output of `currentValue` (a `ModTime`), plus the minimum value of `Revision` which would return `True`. Another way of stating that is you record the revision you last built at, and the revision you last changed at.

Absolutely key to have both, so you can use your change when comparing against upstream, and your built when comparing against downstream.

But each file might be quite big, so we really only want to store them once and refer to them by a small identifier:

    FileId := (File, ModTime, Revision, Revision, [FileId])

And that is what Shake uses internally.

**Implementing the Model**

For those who like very concrete details, which might change at any point in the future, the actual definition is in [Development.Shake.Database](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Database.hs#L107):

    data Result = Result
        {result :: Value -- the result associated with the Key
        ,built :: !Step -- when it was actually run
        ,changed :: !Step -- the step for deciding if it's valid
        ,depends :: [[Id]] -- dependencies
        ,execution :: {-# UNPACK #-} !Float -- duration of last run
        ,traces :: [Trace] -- a trace of the expensive operations
        } deriving Show

The diff from the conceptual model is:

* `ModTime` became `Value`, `Revision` became `Step`, `FileId` became `Id`.
* The `File` is not stored in `Result` since we keep a separate bidirectional mapping of between `FileId` and `File`. Partly this is done so it is easier to assign a `FileId` before we have built the file, and partly because we need to map both ways.
* The dependencies are stored as a list of lists, so we still have access to the parallelism provided by `need`, and if we start rebuilding some dependencies we can do so in parallel.
* We also store `execution` and `traces` so we can produce profiling reports.
