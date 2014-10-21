# Safe unsafePerformIO

I think sometimes to get a robust abstraction you need to use unsafePerformIO internally. As an example, I have a handful of uses in Shake - see https://github.com/ndmitchell/shake/search?q=unsafePerformIO - they break down into:

* Paths.hs - a build time hack because of things I don't have control over - this is one I would love to get rid of if I could.
* Test/Journal.hs - testing code, since I want to observe something that is deliberately not observable in the normal code.
* General/Timing.hs - global profiling code, where it's essential everything shares the same profiling information. Passing around the state would break the information I want to observe.
* Development/Shake/Value.hs - a witness cache, because its essentially working around the fact that type instances aren't first class and queryable.
* Development/Shake/Resource.hs - a unique counter, internally hidden, so I can have an Ord on a type and thus detect deadlocks. I could certainly imagine a library for requesting unique numbers (in fact, I'm sure there must be several) that did this for me. Internally, I'd expect that to use unsafePerformIO.
* Development/Shake/Progress.hs - a cache of an environment variable to improve performance.

I think I would argue in favour of each of those to Lennart (who is my standard "you shouldn't be doing that you idiot" sounding board).