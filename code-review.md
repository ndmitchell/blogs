# Code Review Reviewed

_Summary: I used to be mildly against code review on all merges. Now I'm for it. Code review is a good idea for knowledge sharing, not spotting bugs._

For my open-source projects, I accept pull requests from external contributors which I review, but my contributions are probably not looked at by anyone else. I suspect most non-huge open-source projects follow the same pattern. For larger projects code review seems more common, but not something I've done during most of my career.

Until recently, I was a favour of occasional code review, e.g. towards the end of a project taking a look at the code. The problem with that approach is that it is hard to find the right time - at the end there is little appetite for large-scale refactoring and when the project is busy cranking out code there is no time and the code is still changing. While I was a fan of this idea in theory, it never worked in practice.

Some teams use code review on every merge to master, perhaps several times per person per day. That's something I used to disagree with, but I asked some smart people, and their explanations changed my opinion. The important realisation was thinking about what code review is for.

* **Checking the code works: NO.** Code review is not about checking the code works and is bug free. I have tests, and leverage the type system, which is meant to convince me and other readers that my code is correct. For certain very sensitive bits a second set of eyes checking for bugs may be useful, but if all your code is that sensitive it will also be bug-ridden. However, code review may spot the absence or inadequacy of tests.

* **Checking simple rules and conventions: NO.** Often projects have rules about which packages/extensions can be used, tab width and line length etc. While these checks can be carried out by a human, they are much better automated. Even when "code reviewing" students work at York University (aka marking) it quickly became clear that I was wasting time on trivialities, and thus wrote [HLint](https://github.com/ndmitchell/hlint) to automate it.

* **High-level design: A BIT.** Code review should think about the high-level design (e.g. should this be a web server or command line tool), but often the code is too detailed to properly elucidate these choices. High-level design should usually be done on a bug tracker a whiteboard before writing code.

* **Mid-level design: YES.** Code review is often very good at challenging details around the mid-level design - e.g. does the choice of `Text` vs `ByteString` make sense, is there some abstraction that needs introducing. The author of some code is often heavily influenced by the journey they took to get to some point, by seeing code without that history a reviewer can often suggest a better approach.

* **Spreading knowledge: YES.** Code review is great for spreading knowledge of the code to others in the team. Code review requires the code to be written in such a way that someone else can understand it, and ensures that there is someone else who is familiar with it. For larger projects that's invaluable when someone goes on holiday.

* **Team cohesion: YES** Code review requires different members of the team to share their work with each other, and to have an appreciation of what other people are working through and the challenges it presents. It's all too easy to declare a problem "easy" without understanding the details, and code review removes that temptation.

* **Training: YES.** Code review is also good about teaching new techniques and approaches. As an example, there are many generics libraries in Haskell, and many Haskell programmers are familiar with at most two of them. Sometimes a reviewer can use their particular flavour of generics to suggest improvements.

In contrast, I think the disadvantages of code review all revolve around the time required:

1. It takes other peoples time to do the review, when they could be doing something else.
2. It takes your time waiting for a review - often the next step is hard to do without the previous step being agreed. As a result, I think code review work should be prioritised.
3. As a consequence of the two previous issues, code review changes the "unit of work", leading to more changes in a single bundle.

These issues are real, but I think the benefit of knowledge sharing outweighs the cost.

In my career I have seen several instances of bad code review, where code review trends dangerously close to personal attacks, or was only concerned with policing code style. While rethinking code review has convinced me it is worthwhile, it certainly hasn't convinced me that doing it right is easy.
