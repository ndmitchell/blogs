# Shake 0.19

_Summary: The new version of Shake has some tweaks to how stdin works with `cmd`._

I've just released [Shake 0.19](https://shakebuild.com/), see the [full change log](https://github.com/ndmitchell/shake/blob/master/CHANGES.txt). Most of the interesting changes in this release are around the `cmd`/`command` functions, which let you easily run command lines. As an example, Shake has always allowed:

```haskell
cmd "gcc -c" [source] "-o" [output]
```

This snippet compiles a source file using `gcc`. The [`cmd` function](https://hackage.haskell.org/package/shake/docs/Development-Shake-Command.html#v:cmd) is variadic, and treats strings as space-separated arguments, and lists as literal arguments. It's overloaded by return type, so can work in the `IO` monad (entirely outside Shake) or the Shake `Action` monad (inside Shake). You can capture results and pass in options, e.g. to get the standard error and run in a different directory, you can do:

```haskell
Stderr err <- cmd "gcc -c" [source] "-o" [output] (Cwd "src")
```

Shake is a dynamic build system with advanced dependency tracking features that let's you write your rules in Haskell. It just so happens that running commands is _very_ common in build systems, so while not really part of a build system, it's a part of Shake that has had a lot of work done on it. Since the command is both ergonomic and featureful, I've taken to using the [module `Develoment.Shake.Command`](https://hackage.haskell.org/package/shake/docs/Development-Shake-Command.html) in non-Shake related projects.

**Recent `cmd` changes**

The first API breaking change only impacts users of the [file access tracing](https://neilmitchell.blogspot.com/2020/05/file-tracing.html). The resulting type is now polymorphic, and if you opt to for the `FSATrace ByteString`, you'll get your results a few milliseconds faster. Even if you stick with `FSATrace FilePath`, you'll get your results faster than the previous version. Performance of tracing happened to matter for a project I've been working on :-).

The other changes in this release are to process groups and the standard input. In Shake 0.18.3, changes were made to switch to [`create_group=True`](https://hackage.haskell.org/package/process/docs/System-Process.html#t:CreateProcess) in the [process library](https://hackage.haskell.org/package/process), as that improves the ability to cancel actions and clean up sub-processes properly. Unfortunately, on Linux that caused [processes that read from standard input to hang](https://github.com/ndmitchell/shake/issues/748). The correlation between these events, and the exact circumstances that triggered it, took a long time to track down - thanks to [Gergő Érdi](https://gergo.erdi.hu/) for some [excellent bisection work](https://github.com/ndmitchell/shake/issues/748#issuecomment-596450021). Most processes that are run in a build system _should not_ access the standard input, and the only reports have come from `docker` (don't use `-i`) and `ffmpeg` (pass `-nostdin`), but hanging is a very impolite way to fail. In older versions of Shake we inherited the Shake stdin to the child (unless you specified the stdin explicitly [with `Stdin`](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:Stdin)), but now we create a new pipe with no contents. There are now options [`NoProcessGroup`](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:NoProcessGroup) and [`InheritStdin`](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:InheritStdin) which let you change these settings independently. I suspect a handful of commands will need flag tweaks to stop reading the stdin, but they will probably fail saying the stdin is inaccessible, so debugging it should be relatively easy.

In another tale of `cmd` not working how you might hope, in Shake 0.15.2 we changed `cmd` to [close file handles](https://hackage.haskell.org/package/process-1.6.9.0/docs/System-Process.html#v:close_fds) when spawning a process. Unfortunately, that step is _O(n)_ in the number of potential handles on your system, where _n_ is `RLIMIT_NOFILE` and can be quite big, so we switched back in 0.18.4. Since 0.18.4 you can pass `CloseFileHandles` if you definitely want handles to be closed. It's been argued that [`fork` is a bad design](https://www.microsoft.com/en-us/research/publication/a-fork-in-the-road/), and this performance vs safety trade-off seems another point in favour of that viewpoint.

The amount of work that has gone into processes, especially around timeout and cross-platform differences, has been huge. I see 264 commits to these files, but the debugging time associated with them has been many weeks!

**Other changes**

This release contains other little tweaks that might be useful:

* Time spent in the [`batch` function](https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:batch) is better accounted for in profiles.
* Finally deleted the stuff that has been deprecated since 2014, particularly the `*>` operator. I think a six year deprecation cycle seems more than fair for a pre-1.0 library.
* Optimised modification time on Linux.
