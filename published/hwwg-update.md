# Haskell Website Working Group - Update

_Summary: We have agreed a set of principles for the website and are collecting information.._

I'm writing this partly in my capacity as the chair of the [Haskell Website Working Group](https://github.com/ndmitchell/hwwg), and partly as an individual (so blame me rather than the committee). It's fair to say that the original goal of the committee was to make sure everyone agrees on the download page. Discussions amongst the committee lead to a shared goal that the download page itself should clearly direct users along a precise path, without requiring beginners to make decisions requiring judgement. That probably means that download page should only describe one installation path, pushing alternatives onto a separate page.

To decide _what_ should go on the download page, our first step was to evaluate what was currently available, and what experience a beginner might have. We've started that [on this page](https://github.com/ndmitchell/hwwg/blob/master/Steps.md). As an example, it says how to install Haskell, how to open ghci, how to install a tool etc - all using the different options.

When I actually tried installing and using the various options listed on the [current download page](https://www.haskell.org/downloads), they all had confusing steps, unintuitive behaviour and problems that I had to overcome. As an example, Stack on Windows recommends using the 32bit version, while noting that only the 64bit version works. At the same time, Core Platform starts by  telling me to edit a global Cabal config file and then `ghc` crashes at startup.

I invite everyone to help contribute to that page, via pull requests. At the same time, it would be great if the issues raised could be ironed out, leading to a smooth beginner experience (I'm talking to maintainers in person and raising GitHub tickets). Once the information is collected, and ideally the tools have improved, it will be time to make a decision between the options. When the decision comes, it will be technically motivated, and hopefully unambiguous.
