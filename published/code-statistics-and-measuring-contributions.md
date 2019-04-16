# Code Statistics and Measuring Contributions

_Summary: The only way to understand a code base is to ask someone who works on it._

This weekend a relative asked me how can we tell who wrote the code behind the [black hole image](https://eventhorizontelescope.org/), and was interested in the stats available on [GitHub](https://github.com/). There are lots of available stats, but almost every stat can be misleading in some circumstances. The only people who have the context to interpret the stats are those who work on the project, hence my default approach to assessing a project is to _ask someone who works on it_, with the understanding that _they_ may look at relevant stats on GitHub or similar. In this post lets go through some of the reasons that a simplistic interpretation of the stats is often wrong.

These remarks all apply whether you're trying to assign credit for a photo, trying to do performance reviews for an employee or trying to manage a software project.

**What to measure**

There are broadly two ways to measure activity on the code in a repo. The first is additions/deletions of lines of code, where a modified line of code is usually measured as an addition and deletion. The second is number of commits or pull requests, which measures how many batches of changes are made. The problem with the latter is that different people have different styles - some produce big batches, some tiny batches - a factor of 10 between different developers is not unusual. There are also policy reasons that commits may be misleading - some projects squash multiple commits down to one when merging. The number of lines of code gives a better measure of what has changed, but it's merely better, not good - the rest of this post assumes people are focusing on number of lines of code changed.

**All code is equal**

Treating number of lines changed as the contribution assumes that every line is equally hard - but that's far from the case. At a previous company I worked on code that ranged from the internals of a compiler, to intricate C++ libraries, to Haskell GUI's. I estimate that I could produce 100x the volume of Haskell GUI's compared to C++ libraries. Other colleagues worked only only on the compiler, or only on GUIs - vastly changing how much code they produced per hour.

Similarly, each line of code is not equally important. Last week I wrote a few 100 lines of code. Of those, nearly all were done on Monday, and the remainder of the week involved a single line that is ferociously difficult with lots of obscure side conditions (libraries and link order...). That one line is super valuable, but simplistic measuring suggests I napped all Tuesday and Wednesday.

**Code is attributed properly**

Developers typically have user handles or email addresses that are used for code contributions. I currently have at least [two](https://github.com/ndmitchell/) [handles](https://github.com/neil-da/), and in the past when we did stats on a `$WORK` project there were 6 different email addresses that I claimed ownership of. As a consequence, my work shows up under lots of different names, and counting it can be difficult. The longer a project runs, the more chance of developers changing identity.

**The person who changed code did the work**

A big part of software engineering is making old code obsolete. I was recently involved in deleting many thousands of lines that was no longer necessary. With a small team, we created a new project, implemented it, converted 90% of the uses over to the new code, and then stopped. Separately, someone else did the last 10% of the conversion, and then was able to delete a huge number of lines of code. There was definitely work in deleting the final bit of code, but the "labour" involved in that final deletion was mostly carried out months ago by others.

Similarly, when copying a new project in (often called vendoring) there is a big commit to add a lot of new code that was originally written by others, but which gets attributed to a named individual.

**All code is in one place**

Often projects will incorporate library code. For example, the official contribution of [Niklas Broberg](http://www.nbroberg.se/) to [HLint](https://github.com/ndmitchell/hlint) is 8 lines. However, he's called out explicitly [in the README](https://github.com/ndmitchell/hlint#acknowledgements) as someone whose contributions were essential to the project. In this case, because he wrote a library called [`haskell-src-exts`](https://github.com/haskell-suite/haskell-src-exts) without which HLint could not exist, and then continued to improve it for the benefit of HLint for many years.

Furthermore, projects like HLint rely on a compiler, libraries, operating system, and even a version control system. Usually these get overlooked when giving credit since they are relatively old and shared between many projects - but they are an essential part of getting things to work.

**More code is better**

The only purpose of code is to do a thing - whatever that thing might be. In all other ways, code is a liability - it has to be read, tested, compiled etc. Given the choice between 10 lines or 1,000,000 lines of code, I would always prefer 10 lines if they did the same thing. A smarter programmer who can do more with less lines of code is better. The famous quote attributed to [Bill Gates](https://www.goodreads.com/quotes/536587-measuring-programming-progress-by-lines-of-code-is-like-measuring) is still true many decades later:

> Measuring programming progress by lines of code is like measuring aircraft building progress by weight.

**Code is the essence**

Measuring code suggests that code is the thing that matters. The code certainly does matter, but the code is just a representation of an underlying algorithm. The code follows a high-level design. Often much more significant contributions are made by picking the right design, algorithm, approach etc.

**Code is all that matters**

In a large project there is code, but the code doesn't exist in a vacuum. There are other code-adjacent tasks to be performed - discussions, mentoring, teaching, reporting issues, buying computers etc. Many of these are never reflected in the code, yet if omitted, the code wouldn't happen, or would happen slower.
