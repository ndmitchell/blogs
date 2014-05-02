# Shake vs Ninja performance

_Summary: Ninja is a build system focused on being fast. Some limited benchmarking suggests the Shake build system might be faster._

The [Shake build tool](https://github.com/ndmitchell/shake#readme) aims to be both [powerful and fast](https://github.com/ndmitchell/shake/blob/master/docs/Why.md#readme). The [Ninja build tool](http://martine.github.io/ninja/) describes itself as small with a focus on speed. Now that Shake can [run Ninja build files](https://github.com/ndmitchell/shake/blob/master/docs/Ninja.md#readme), I benchmarked Shake against Ninja.

**The Test**

I have been benchmarking Shake vs Ninja as part of the [Shake continuous integration tests](https://travis-ci.org/ndmitchell/shake). [My benchmark](https://github.com/ndmitchell/shake/blob/master/travis.hs) builds the Ninja source code using their Ninja build file, once from scratch with 3 threads (a full build), then builds again to ensure it is up to date (a zero build). The test is run with both Ninja and Shake, always immediately after a Ninja build (to ensure all files are cached). The average times over 71 runs are:

* Full build: Ninja takes 6.552s, Shake takes 5.985s. _Shake is 0.567s faster_.
* Zero build: Ninja takes 0.007s, Shake takes 0.012s. _Ninja is 0.005s faster_.

These tests are run on Linux, on a [Travis](https://travis-ci.org/) machine.  Both the hardware and loading of the machine is likely to vary over time. I deliberately picked a lower level of parallelism to try and ensure the build was not limited by running too many processes (it does not seem to be). It is now a test failure if Shake is slower for the full build, or if Shake is more than 0.1s slower for the zero build.

A more interesting test would be building something more substantial than Ninja - but choosing a benchmark is hard, and I am limited by the amount of Travis time I can use. It is not clear if Shake will be consistently N seconds faster than Ninja, or N% faster than Ninja, or if this result is an aberration due to the particular choice of benchmark. Shake does not implement the Ninja feature of rebuilding when the command line changes - adding that feature would be unlikely to have any impact on the full build but may slightly slow down the Shake zero build.

**Improvements to Shake**

When I first started benchmarking Shake vs Ninja, I had reports that Shake was significantly slower - taking around 40% longer to build large projects. As a result I made a number of improvements to Shake:

**Improvement 1: --skip-commands**

I added the `--skip-commands` flag and `shakeRunCommands` option to Shake, which skips running any `command` operations which have no return results. Provided your build system does not delete temporary files, it allows you to build normally, then build with `--always-make --skip-commands` to "run the build", skipping running commands, measuring the rest of the build system.

**Improvement 2: Makefile parsing**

Using `--always-make --skip-commands` on [LLVM via Ninja files](http://neilmitchell.blogspot.co.uk/2013/06/building-llvm-using-shake.html), I found the non-command build time was 58s. Profiling showed that most of the time was spent parsing Makefiles, so I wrote optimised routines, available from [Development.Shake.Util](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake-Util.html). These changes reduced the LLVM  non-command time to 15s.

**Improvement 3: Filepath normalisation**

Further profiling showed that filepath normalisation was now a bottleneck. I responded by both optimising the filepath normalisation (writing a large test suite and correcting several bugs in the process), and removing some redundant normalisations. These changes reduced the LLVM time to 4s, most of which went on file modification time checking.

**Improvement 4: Turning off idle garbage collection**

By default, programs compiled with GHC run the garbage collector if the Haskell runtime is idle for 0.3s. For Shake, which regularly becomes idle when running commands, the garbage collector ends up competing with the commands it has spawned. I now recommend people [turn off the idle garbage collector](https://github.com/ndmitchell/shake/blob/master/docs/Manual.md#compiling-the-build-system) by compiling with `-with-rtsopts=-I0`, and I do so for the `shake` executable.

**Improvement 5: --timings**

In order to accurately measure where time was going, I added the `--timings` flag and `shakeTimings` option. When run with `--timings` Shake prints out something like:

    Start                             0.006s    1%
    shakeArgsWith                     0.000s    0%
    Function shake                    0.002s    0%
    Database read                     0.049s   10%  ===
    With database                     0.002s    0%
    Running rules                     0.353s   72%  =========================
    Pool finished (5 threads, 2 max)  0.000s    0%
    Lint checking                     0.033s    6%  ==
    Profile report                    0.041s    8%  ==
    Total                             0.486s  100%
    Build completed in 0:01m

Here we can see which stages are taking most time. For example, reading in the database takes 0.049s at 10% of the time. The `=` symbols to the right serve as a crude bar plot representing the timings.

**Improvement 6: Smaller database**

For zero builds I found much of the time was spent reading the database. I changed some of the representation, using smaller `Int` types and more compact encodings. These changes reduced the database by ~25% and had a small effect on the time to read the database.

**Future improvements**

For the full build, I beat Ninja, despite originally only aiming for a draw. The build overhead introduced by Shake is 0.029s, of which 0.010s is running the rules. Provided that scales linearly, the cost seems negligible compared to actually performing the build.

For the zero build, I am slower than Ninja. To investigate I measured just running `--version` with Ninja and Shake. Ninja takes 0.003s and Shake takes 0.004s, so a large portion of the zero build times is the cost of starting the executable, not project specific. Running `--timing` I see:

    Start                             0.000s    3%  ==                       
    shakeArgsWith                     0.000s    7%  =====                    
    Ninja parse                       0.001s   16%  ===========              
    Function shake                    0.000s   10%  ======                   
    Database read                     0.002s   36%  =========================
    With database                     0.000s    3%  ==                       
    Running rules                     0.001s   20%  =============            
    Pool finished (2 threads, 2 max)  0.000s    2%  =                        
    Total                             0.004s  100%                           

The largest bottleneck is database reading. [Duncan Coutts'](http://www.well-typed.com/people/duncan/) recent work on moving the [binary](http://hackage.haskell.org/packages/binary) library to [CBOR](https://tools.ietf.org/html/rfc7049) is likely to result in a smaller and faster database. I await that work eagerly, and will look at further optimisations after it is available.

**Is Shake faster than Ninja?**

For building Ninja from scratch, Shake is faster than Ninja (perhaps the Ninja developers should switch to Shake for their development work :P). Another Ninja user [benchmarked](https://github.com/ndmitchell/shake/issues/25#issuecomment-28010697) building the [VXL project](http://vxl.sourceforge.net/) with both Shake and Ninja and discovered Ninja took 44 mins, while Shake took 41 mins (Shake 3 minutes faster) - but this benchmark was only run a handful of times. I would be interested to hear [additional results](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).
