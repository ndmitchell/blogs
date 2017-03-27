# Code Review Reviewed

_Summary: I used to be mildly against code review for all merges. Now I'm for it. Code review is a good idea for knowledge sharing, not spotting bugs._

For my open-source projects, I accept pull requests from external contributors and review them, but my contributions are probably not looked at by anyone else. I suspect most non-huge open-source projects follow the same pattern. For larger projects (which in my personal experience have always been closed source), code review is more common, but something I've not done at many companies.

Until recently, I was a favour of occasional code review, e.g. towards the end of a project taking a look at the code. The problem with that approach is that it is hard to find the right time - at the end there is little appetite for large-scale refactoring and when the project is busy cranking out code there is no time and the code is still changing. While I was a fan of this idea in theory, it never worked in practice.

Some teams use code review on every merge to master, perhaps several times a day. That's something I used to disagree with, but I asked some smart people, and their explanations convinced me. The important step was thinking about what code review is for.

* **Checking the code works: NO.** Code review is not about checking the code works and is bug free. I have tests, and leverage the type system, which is meant to convince me and other readers that my code is correct. For certain very sensitive bits a second set of eyes checking for bugs may be useful, but if all your code is that sensitive it will also be bug-ridden. However, code review may spot the absence or inadequacy of tests.

* **Checking simple rules and conventions: NO.** Often projects have rules about which packages/extensions can be used, tab width and line length etc. While these checks can be carried out by a human, they are much better automated. Even when "code reviewing" students work at York University (aka marking) it quickly became clear that I was wasting time on trivialities, and thus wrote HLint to automate it.

* **High-level design: A BIT.** Code review should be a bit about high-level design, but to get the code at a point where the high-level design is still malleable often it's too late by the time there is code. That design should be done on some kind of bug tracker or a whiteboard and the code should be looking at more technical details.

* **Mid-level design: YES.** Code review serves as training, so people can get different opinions. There will often be a better way of doing it, and often someone won't know that better way, so it acts as a learning exercise.

* **Spreading knowledge: YES.** Code review spreads knowledge. It requires the code to be written in such a way that someone else can understand it, and ensures that there is someone else who has thoroughly read all the details. That's invaluable for larger projects where often someone goes on holiday or similar. In my opinion, this is the biggest benefit of code review, and one I'd entirely neglected.

There are three obvious disadvantages of code review:

1. It takes other peoples time to do the review, when they could be doing something else.
2. It takes your time waiting for a review - often the next step is hard to do without the previous step being agreed. As a result, I think code review work should be prioritised.
3. As a consequence of the two previous issues, code review changes the "unit of work", leading to more changes in a single bundle.

One issue code review can exacerbate is people feeling attacked at their code - which is a real issue - but hopefully less problematic.

In many cases I had seen _bad_ code review, which had confused what I was after.