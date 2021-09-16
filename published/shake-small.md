# Small Project Build Systems

_Summary: Forward build systems might work better for small projects._

[Yesterday's post](https://neilmitchell.blogspot.com/2021/09/reflecting-on-shake-build-system.html) talked about how Shake is a good medium sized build system - but what about smaller projects? Is the Shake model right for them? Shake can be considered a _backwards build system_. Each rule says how to produce a file, given some input files (which are dependencies) and an action. Almost all build systems (e.g. Make, Buck, Bazel, CMake, SCons, Ninja) fit this model, which is analysed in the [Build Systems a la Carte paper](https://ndmitchell.com/#shake_21_apr_2020). While this model works, it has two disadvantages:

* You have to explicitly list dependencies, or infer them from include files etc. That means either dependencies are insufficient (you probably forgot some), or they are excessive (you added some you don't need). Usually both.
* You have to think backwards. When you ask someone how to build an executable from a C file, no one talks about linking first, but to program a build system you have to.

The alternative to a backwards build system is a _forwards build system_, of which [Memoize](https://github.com/kgaughan/memoize.py) was the first. You just write out the commands in order, and dependency tracing figures out if they have changed. To compile a C program it can be as simple as:

```
gcc -c util.c
gcc -c main.c
gcc -o main main.o util.o
```

That build script is incredibly simple - so simple it could also be treated as a shell script.

A few years ago I wrote such a system, called [Rattle](https://github.com/ndmitchell/rattle), and wrote a paper about it at [OOPSLA 2020](https://ndmitchell.com/#rattle_18_nov_2020) with my co-authors Sarah Spall and Sam Tobin-Hochstadt. Sarah gave a talk about Rattle [at OOPSLA](https://www.youtube.com/watch?v=WRLfQo-IJTg), and I gave a talk [at Build Meetup 2021](https://ndmitchell.com/#rattle_25_jun_2021). We were able to compile projects like NodeJS faster than the NodeJS build system (which uses Make), showing the idea might be feasible.

If forward build systems are so great, why do I think they are most suitable for small projects? There are four reasons, the first three of which have mitigations, but the final one sets a limit on the size at which forward build systems are suitable.

1. Forward build systems rely on tracing which files are dependencies of a command. Doing that quickly in a cross-platform manner is a nightmare. There are tricks like hooking system calls etc, but it presents a significant engineering hurdle, especially on MacOS, which makes this task harder with every release.
2. Forward build systems are immature. The earliest examples no longer work. Rattle is a relatively practical research system - it could evolve into a production system - but it's not there yet. And compared to the alternatives, Rattle is probably one of the closest to production, in large part because it builds off a lot of common infrastructure from Shake which is much more mature.
3. Forward build systems lack parallelism, since if you want to express parallelism, you need to think about dependencies once more, and it's easy to go wrong. Rattle mostly solves the parallelism by automatically inferring when it is safe to parallelise, which is how we were able to remain competitive with Make.

And finally, the biggest issue is that forward build systems are not compositional, while backward build systems are. If you want to write a 1 million build rule system, in a backwards system, each rule looks like any other. Whereas in a forward build system, assuming you need to give an order, writing down that order in a compositional way is hard - in fact, whenever I've tried it, you start expressing the dependencies between entries and end up with a backwards build system.

Happily, people are continuing to research forward build system. Rattle adds parallelism, [Stroll](https://blogs.ncl.ac.uk/andreymokhov/stroll/) removes the requirement for an order, [Fac](https://sites.science.oregonstate.edu/~roundyd/fac/) allows some dependencies and infers the remaining ones, [LaForge](https://arxiv.org/pdf/2108.12469.pdf) finds greater incrementality. Perhaps all those ideas can be combined, along with a lot of engineering, to produce a practical forward build system.

Rattle has shown a well engineered forward build system would be feasible for small projects. It's unclear how much larger the concept might be able to scale, probably never to millions of files, but for small projects it might provide a significantly lower effort path to writing build systems.
