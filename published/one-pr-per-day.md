# The One PR Per Day Rule

_Summary: The rough rule I use for teams I'm on is make at least one PR per day._

One of the principles I've used quite successfully in a number of teams I've been involved with is:

> Make at least one Pull Request per day

This principle nicely captures a number of development practices I consider important.

* **Most things should be reflected in code.** If you spend a day coding, improving documentation, writing tests etc. there is a natural reflection in the code. If you spend a day helping someone through some problems, that probably indicates there is better documentation to be written. If you spend a day doing dev-ops, that should probably be reflected with Terraform files or similar. Not everything that matters produces code (e.g. organising an office party, immigration paperwork, attending a conference), but most things do.

* **Work incrementally.** If a piece of code takes more than one day, it's a good idea to split it into smaller pieces that can land incrementally. It's always possible that after a few days work you'll realise your overarching idea wasn't great, but if you've polished up some libraries and added tests along the way, that still produced value.

* **Work with autonomy.** I'm a big fan of giving developers as much autonomy as possible - discuss the broad goals and then let them figure out the details. However, with such freedom, it's much easier for things to go off in the wrong direction. Seeing incremental pieces of code every day gives a fairly good direction indicator, and allows problems to surface before a massive time investment.

* **Write reviewable code.** If you have 20K lines in one big blob, there's no realistic way to review it. By splitting code into smaller, manageable, independent units it's much easier to review. More importantly, the reviewer should be able to say "No, that's not a good idea" - doing that to a days work is sad, doing it to a whole months work is brutal.

* **Foster collaboration.** In a rapidly moving project, it's important that everyone is benefiting from other peoples incremental improvements, as otherwise everyone solves the same problems. By getting the code merged every day it's much easier for different people to contribute to an area of the code base, avoiding the problem of others staying away from a piece of code that someone else is working on.

* **Get feedback.** If the end user is able to test the results every day that's even better, as it means they can be involved in the feedback loop - potentially refining what they actually want.

The "rule" isn't really a rule, it's more a statement of culture and principles, but one I have found concise and simple to explain. While I like this as a statement of culture, I _do not_ measure it, as that would create all the wrong incentives.
