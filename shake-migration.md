# Converting Make to Shake

_Summary: I have converted over 10,000 lines from Make to Shake. Here are some tips I learnt along the way._

Make is the de facto build system for large projects - if no one made an active choice, your project is probably using Make. The [Shake build system](https://github.com/ndmitchell/shake#readme) can be a better alternative, but how should you convert? The following tips are based on my experience converting a 10,000 line Make system to Shake.

**Shake can do whatever Make can**

Shake is more powerful than Make, so if Make could do something, Shake probably can too. As a first approximation, the Make snippet:

    output: input1 input2
        shell command to run

Becomes:

    "output" *> \out -> do
        need ["input1","input2"]
        cmd Shell "shell command to run"

In addition:

* Variables in Make usually map to normal Haskell variables.
* Definitions of rules and dependencies use the functions from `Development.Shake`. For example, `.PHONY` maps to the `phony` function.
* Filepath manipulation uses the functions from `Development.Shake.FilePath`.
* Dynamically generated include files can be handled with `needMakefileDependencies` from `Development.Shake.Util`.

**Preserve the file/directory structure**

The existing Make system will generate object files with particular names in particular places. Often these locations aren't what you would pick if you wrote the build system afresh. However, resist the temptation to "clean up" these pieces during the conversion. Treat the file locations as a specification, which lets you focus on the conversion to Shake without simultaneously redesigning a large and complex build system.

**Treat the Makefile as a black box**

Often the existing Makefile will be hard to understand, and sometimes won't be worth reading at all. The most important information in the Makefile is what commands it runs, which can be determined by `make clean && make -j1 > log.txt`, which captures a complete list of the commands run. From the commands it is usually relatively easy to determine the inputs and outputs, from which you can write the Shake rules. However, the Makefile can be useful to figure out which commands to group into a single rule, and how to generalise rules to cover multiple files.

**Split the metadata from the logic**

Often the Makefiles combine metadata (these object files go into this executable) with logic (use `gcc -O2` to build all executables). Shake is great for writing build logic, but metadata is often better placed in separate files (the Haskell syntax can be a little heavy). You can use the full power of Haskell to store whatever metadata you require, and `addOracle` from Shake can introduce granular dependencies on the information. The module `Development.Shake.Config` provides some helper functions that might serve as a suitable base.

To bootstrap the Shake system, often the metadata can be extracted from the existing Makefiles. You can write a temporary script to parse the Makefile and extract whatever you consider the metadata, clean it up, and write it to new configuration files. Initially the config files are generated, but once you delete the Make original, they become source files.

**Focus on a single platform/configuration**

Often a build system will be cross-platform (Linux/Mac/Windows), build multiple targets (binaries/distribution package/documentation) and build multiple configurations (release/debug/profile). To start the conversion, focus only on the most heavily developed platform/configuration - if the migration is successful, abstracting over the differences is far easier in Shake than Make. You may wish to start with a simple target to try out Shake (e.g. documentation), but after that work on the target developers use every day, so that the developers can make use of the improvements sooner, motivating the migration.

**Convert bottom up**

Shake demands that it built all the dependencies (it checks the modification time is _equal_ to what it remembered), in contrast Make only requires that targets are newer than their dependencies. As a result, you should start converting the leaves of the build system to Shake, and work upwards. Provided you use the same file/directory structure, you can then build what you have defined with Shake, then finish the build with Make, checking the result still works as expected.

**Run Make and Shake in parallel**

One you have migrated enough of the build system to be useful (the usual targets in the most common configuration), you should encourage some developers to try Shake instead of Make. These developers will find things that don't work properly, hidden features in the Make system that no one knew about etc. Expect to fix problems and iterate several times.

Hopefully the Shake system will be faster and more robust. Once these advantages have encouraged all the main developers to make the switch, you should delete/disable the Make system and expect it to bitrot quickly.

**Refactor individual rules**

As you are converting rules from Make to Shake you can translate them directly and refactor later, or convert straight into more idiomatic Shake. As an example, you might start with:

    cmd Shell "ls >" out

The argument `Shell` tells Shake to use the system shell, meaning that `>` redirect works. Later on you may wish to switch to:

    Stdout result <- cmd "ls"
    writeFile' out result

Now you are invoking the `ls` command directly, capturing the output using Shake. Sometime later you may switch to:

    getDirectoryFiles "." ["*"]

Which is the Shake tracked way of getting a list of files. Similarly, calling `sed` or `for` through `Shell` should probably be gradually converted to Shake/Haskell operations.

**Refactor the whole**

Once you have converted the whole build system, and disabled the original Make system, you may wish to refactor the build system - putting files in more appropriate places, rethinking file dependencies etc. In truth, I've never got round to this step, and I would be surprised if many people did. However, as the build system grows, hopefully the new bits with sensible decisions will gradually begin to outnumber the old bits with questionable design.

**Ask if you get stuck**

Build systems (even in Shake) are complex entities, with intricate coordination between files, which mostly run untyped external commands with many platform/version differences. As a result, build systems are often complex to write.

If you have a problem using Shake, just ask. If you can boil down the problem to something fairly standalone, ask on [StackOverflow](http://stackoverflow.com/questions/tagged/shake-build-system) with the tag `shake-build-system`. If you are looking for more general advice, ask on the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system). If you succeed, write a blog post and [tweet me](https://twitter.com/ndm_haskell).
