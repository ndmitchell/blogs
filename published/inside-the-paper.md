# Inside the paper: Build Systems a la Carte

_Summary: How we went about writing a build systems comparison paper, how we learnt what we learnt, and why the end result surprised us. A glimpse inside writing a paper._

The [final version of the Build Systems a la Carte paper](https://github.com/snowleopard/build/releases/download/icfp-final/build-systems.pdf) has just been sent to [ICFP 2018](https://icfp18.sigplan.org/) - see [an overview from one of my co-authors](https://blogs.ncl.ac.uk/andreymokhov/build-systems-a-la-carte/). The paper is a collaboration between [Andrey Mokhov](https://www.ncl.ac.uk/engineering/staff/profile/andreymokhov.html) at Newcastle University, [Simon Peyton Jones](https://www.microsoft.com/en-us/research/people/simonpj/) at Microsoft Research and me (working in industry). Below is the tale of how we discovered what we discovered, hopefully slightly demystifying the research process. While the paper is a collaboration, this blog post is my view and mine alone.

The paper started with the idea of comparing and contrasting build systems. There were two motivating factors, I wanted a blueprint for writing write Cloud Shake, while Simon wanted to compare build systems (Andrey wanted a bit of both). The thing we all agreed on was that Haskell is a great executable specification for describing build systems, and that refactoring is a powerful tool. Armed with that approach, we went off to try and implement various build systems, chosen based on our familiarity with them and the perceived differences between them. You can see our progress [in the git repo](https://github.com/snowleopard/build), starting 20th Feb (less than a month before the ICFP deadline!).

All of us came to the table with some predefined notions of what should and shouldn't be in the model. Andrey brought the `Store` abstraction. I brought the ideas of monadic vs applicative dependencies. We iterated and quickly made our first "breakthrough", a task abstraction which nicely modelled user rules, including the difference between monadic and applicative dependencies:

```haskell
type Task c k v = forall f . c f => (k -> f v) -> (k -> f v)
```

Essentially, given a way to build dependencies, I can give you a way to build keys. By parameterising the `Task` by `c` (of type `Constraint`) we can produce `Task Monad` and `Task Applicative`, nicely capturing the differences in power. It was only later when preparing an [artefact for evaluation](https://icfp18.sigplan.org/track/icfp-2018-Artifact-Evaluation) that we noticed [Docker](https://www.docker.com/) is a `Task Functor` build system.  We made a number of iterations on this `Task` type (adding and removing `newtype`, how to represent input files, where to put the second `k` etc) - but fundamentally had a model to work with.

The next step was to start writing build systems. We picked Make, Shake, Excel, Ninja and Bazel as our first set to get working. Implementing these systems effectively became a four-dimensional optimisation problem:

* Closeness of the model to the underlying system it was based on.
* Simplicity of code for each individual system.
* Reuse of code across build systems.
* Reuse of abstractions across build systems.

The first versions were separate monoliths of code, reusing a handful of helper functions, with a fairly arbitrary set of what to model and what to exclude. Since we had executable specifications, with tests, we came up with possible improvements, tried them, and decided whether to keep them or discard them. We iterated, as individuals, as pairs (all three possible pairs) and as a group - making improvements along various dimensions. For a good few weeks Andrey and myself had competing directories in the repo, with different underlying ideas but stealing refinements from each other. I think there were about 25 separate "breakthroughs" to move the code to where we ended up. As the code became clearer, we started to understand the commonalities behind build systems, which helped the code become clearer - a virtuous cycle.

The most academically interesting breakthrough was to realise that build systems can be split into something that decides what to rebuild, and something that orders the rebuilding, putting build systems in a two-dimensional table. While the result feels natural (if you carefully structure your description of a build system it even falls out grammatically!), it was entirely non-obvious beforehand, and emerged naturally by following the abstraction opportunities presented by the code.

By the end we were able to faithfully model details of Make/Excel/Shake that initially eluded us, with each build system being just two functions, where all functions could be combined to produce working build systems. As an example, Shake is:

```haskell
shake = suspending vtRebuilder
```

The `suspending` is also used by `Nix`, and the `vtRebuilder` is also used by `Ninja`. Shake is just putting two existing things together, so we have great reuse of code and abstractions between build systems. In some places the code is more complex than I'd like, but you can't have everything (or maybe you can - we may well improve the models further).

After submitting the paper to ICFP 2018, we also put a draft online, which led to a deluge of comments from experts in many of the systems we talked about - the acknowledgements in the paper start to show how much excellent feedback we got. The most interesting feedback was that we'd misclassified Bazel - it's actually more like Excel than we realised. What was particularly nice is that our framework was able to describe what we thought Bazel was in enough detail that people involved with Bazel could correct us - a clear sign we were modelling interesting details.

Now that the paper is done, I hope the abstractions can start proving their value. In the context of Shake, I would like it can serve as a design document. Ever since the earliest days of Shake, I've used a two-timestamp approach to recording what happened with a key, as described in S2.3.1 of [the original paper](https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf). Unfortunately, whenever I've tried to explain this trick to someone in person, their eyes glaze over. Fortunately, given a clear model, I now know that what I've really implemented is an optimisation over `vtRebuilder`. Furthermore, I now have the framework to talk about the benefits and costs of the optimisation, making it much easier to understand and justify.

My goal before writing the paper was to turn Shake into Cloud Shake, and the desire to do that in a principled way. Now the paper is finished I can resume that quest, with a fairly good understanding of how to do it. One thing the paper sensibly abstracts over is all the technical details (parallelism, network traffic etc) - armed with the right abstractions those technical details are what I'll be focusing on for Cloud Shake.

Thinking more broadly, the things this paper taught me (or that I already thought but it confirmed):

* Follow the [Simon Peyton Jones how to write a paper guidelines](https://www.cis.upenn.edu/~sweirich/icfp-plmw15/slides/peyton-jones.pdf), of which number 1 is most important. "Writing papers is a primary mechanism for doing research (not just for reporting it)".
* Innovation isn't thinking in isolation, it's thinking of a process that gives you the right problems, the right tools, and the right direction. With those things in place, the chances of ending up somewhere interesting increase dramatically.
* Deadlines spur writing papers. It feels like we should be better, and not need the external deadlines, but it seems to help in practice.
* Simplicity is valuable in its own right. The main research contribution of this paper sounds obvious a minute after explaining it, which makes me very happy.
* Co-authors matter. As a set of co-authors we agree on some things (e.g. Haskell), but disagree strongly on others (e.g. two parallel branches of development, [6 rejected pull requests](https://github.com/snowleopard/build/pulls?q=is%3Apr+is%3Aclosed+is%3Aunmerged%09)). I am sure the paper would have been significantly worse with anyone of us removed (these are my conclusions, I don't guarantee my co-authors agree!).
* Feedback is super valuable, whether it comes from peer reviewers or Reddit comments. The feedback improved the paper, and also provided motivation.

Hopefully this post lets people in on the secret that writing academic papers isn't magic, that papers don't emerge fully formed, and that it involves a lot of work and refinement.
