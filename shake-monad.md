


    build :: FilePath -> BuildM String
    build :: FilePath -> BuildA String

    build "a+b" = liftA2 (++) (build "a") (build "b")
    build "!a" = do name <- build "a"; build name

So the first can be written in either, the second requires monads. The key point here is that applicative lets you get the list of all files in advance, monad doesn't. In return, Monad lets you move data from the String to the FilePath, which the Applicative doesn't.

In practice, no build systems are applicative, they all find ways to sneak in extra power in some places. For example, all of (the ones on the shootout) can implement "!a", which several are notionally based on a graph and you'd expect to have applicative power. What's interesting is _how_ they get around the problem. Some of them do it in different ways per system.

Link to Gergo's blogpost, http://gergo.erdi.hu/blog/2014-07-12-arrow's_place_in_the_applicative/monad_hierarchy/

import Control.Applicative
import Data.Traversable

data File a = File a

instance Functor File
instance Applicative File
instance Monad File


file :: a -> File a
file = File


file1 = file "X"
file2 = file "Y"

list_txt = file [file1, file2]

result1 = tar [file1, file2]

result2 = tar =<< list_txt


tar :: [File a] -> File [a]
tar = sequenceA
