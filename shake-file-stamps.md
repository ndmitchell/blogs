# Are file stamps bad

* Given that you don't know the output name for certain, create a new
file that stores the name in a predictable place. I use roughly that
trick here: https://github.com/ndmitchell/ghc-make/blob/master/Build.hs#L72
. So you would have crate1.txt, which contains the name of the output
file, then run:

  "crate1.txt" *> \out -> do
      realName <- ... -- run command to create the crate, and figure
out where it ended up
      need [realName]
      writeFile' out realName

Then when you want to depend on crate1.txt you can do realName <-
readFile' "create1.txt"; need [realName]. Shake let's you declare
additional dependencies as you are going, which makes this trick work.


As regards the stamp files, I tend to find them to be of benefit when
used in small amounts - they let you easily use rm to force rerunning
parts of the build, particularly while developing rules. I expanded on
my thoughts a bit here:
https://groups.google.com/d/msg/shake-build-system/tOMy7WuDhH0/7YMZV0z9AQgJ

If you ever release something that builds Rust with Shake, or writing
any blog posts etc. on it, please do let me know.

As it stands, Shake doesn't have anything that is only run if the 
dependencies change, but doesn't produce output. There are a few types 
of rule that don't produce output, but they always rerun regardless of 
if the dependencies have changed or not (alwaysRerun, phony, action). 
That is somewhat intentional, as I find that if you can't prod a rule 
to force it to rerun then developing the rule becomes harder. 

In the tup example, if nothing had changed (other than perhaps your 
$PATH variable), how would you force the rerunning of the test? In the 
Shake example you could rm output.run. I've done much the same setup 
as you have, and have found the ability to force rerunning very 
useful. In particular, I usually make output.run contain the stdout of 
the process, namely: 

   "output.run" *> \out -> do 
      need ["output.sh"] 
      Stdout stdout <- cmd "./output.sh" 
      writeFile' out stdout 

I've found that works nicely. I'm also not afraid to use "stamp" files 
which are empty files for helping the build system track stuff - it 
still gives the correct results, but I agree its a bit ugly 
conceptually. 

I think it would be feasible to add an noOutput rule to Shake, and 
wonder if I should do so? 

I also think this would be a good inclusion in the build-shootout. 
Please feel free to raise a ticket, or submit a pull request. 
